open Typedppxlib
open Typedppxlib_ocaml_typing
open Typedppxlib_ocaml_parsing
open Parsetree

let wrap_type_name name = "*MACRO_" ^ name
module Attribute_encoding = struct
  let bug ~loc =
    Location.raise_errorf ~loc "somehow we got an invalid attribute"
  let name ~loc = Location.mkloc "template" loc
  let encode ~loc ~params ~template =
    let open Ast_helper in
    let params =
      let constants =
        List.map
          (fun param ->
            let Location.{ txt = param; loc } = param in
            Exp.constant ~loc (Const.string ~loc param))
          params in
      match constants with
      | [] -> None
      | [constant] -> Some constant
      | constants -> Some (Exp.tuple ~loc constants) in
    let template_eval = Str.eval ~loc template in

    match params with
    | Some params -> PStr [Str.eval ~loc params; template_eval]
    | None -> PStr [template_eval]
  let decode ~loc payload =
    let params, template =
      match payload with
      | PStr
          [
            { pstr_desc = Pstr_eval (params, []); _ };
            { pstr_desc = Pstr_eval (template, []); _ };
          ] ->
        (Some params, template)
      | PStr [{ pstr_desc = Pstr_eval (template, []); _ }] -> (None, template)
      | _ -> bug ~loc in
    let params =
      match params with
      | Some { pexp_desc = Pexp_tuple params; _ } -> params
      | Some param -> [param]
      | None -> [] in
    let params =
      List.map
        (fun param ->
          match param.pexp_desc with
          | Pexp_constant (Pconst_string (param, _, _)) -> param
          | _ -> bug ~loc:param.pexp_loc)
        params in
    (params, template)
end
module Unhandled_extension = struct
  module String_map = Map.Make (String)

  let bug ~loc = Location.raise_errorf ~loc "somehow we got an invalid macro"
  let subst_template ~loc ~params ~args template =
    let map =
      match params with
      | [] -> (
        match args with
        | PStr [] -> String_map.empty
        | _ -> Location.raise_errorf ~loc "expected 0 arguments")
      | [param] ->
        let arg =
          match args with
          | PStr [{ pstr_desc = Pstr_eval (arg, []); _ }] -> arg
          | _ -> Location.raise_errorf ~loc "requires an expression" in
        String_map.(add param arg empty)
      | params ->
        let args =
          match args with
          | PStr
              [
                {
                  pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple args; _ }, []);
                  _;
                };
              ] ->
            args
          | _ -> Location.raise_errorf ~loc "requires a tuple expression" in
        let () =
          let params_length = List.length params in
          let args_length = List.length args in
          if params_length <> args_length then
            Location.raise_errorf ~loc "expected %d arguments, but received %d"
              params_length args_length in
        List.fold_left
          (fun map (param, arg) -> String_map.add param arg map)
          String_map.empty (List.combine params args) in

    let expression super expr =
      let loc = expr.pexp_loc in
      match expr.pexp_desc with
      (* TODO: escape *)
      | Pexp_extension ({ txt = "e"; loc = _ }, payload) -> (
        match payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc = Pexp_ident { txt = Lident param; loc = _ };
                        _;
                      },
                      [] );
                _;
              };
            ] -> (
          match String_map.find_opt param map with
          | Some expr -> expr
          | None ->
            Location.raise_errorf ~loc
              "weird, this should be a parameter, likely it's a bug on \
               ppx_macro")
        | _ ->
          (* TODO: check this on declaration *)
          Location.raise_errorf ~loc "not a simple ident")
      | _ -> Ast_mapper.default_mapper.expr super expr in
    let mapper = { Ast_mapper.default_mapper with expr = expression } in
    mapper.expr mapper template

  let apply_macro ~loc macro args =
    let attr =
      match macro.Types.type_attributes with
      | [attr] -> attr
      | _ -> bug ~loc:macro.type_loc in
    let params, template =
      Attribute_encoding.decode ~loc:attr.attr_loc attr.attr_payload in
    subst_template ~loc ~params ~args template

  let type_extension (super : Hooks.base) ?in_function ~recarg env pexp expected
      ((name, args) as extension) =
    let loc = pexp.pexp_loc in
    let type_name = wrap_type_name name.Location.txt in
    try
      let _path, macro = Env.lookup_type ~loc (Lident type_name) env in
      let pexp = apply_macro ~loc macro args in
      Typecore.type_exp env pexp
    with
    | _exn ->
      super.type_extension ?in_function ~recarg env pexp expected extension
  let hooks = { Hooks.default with type_extension }
end
module Define_macro = struct
  let invalid_input ~loc =
    (* TODO: enhance, by removing this and replacing with precise messages *)
    Location.raise_errorf ~loc
      "expected single binding of the form let%%define [%%NAME x] = x"
  let extract_binding binding =
    let name, params =
      match binding.pvb_pat.ppat_desc with
      | Ppat_extension (name, PStr [{ pstr_desc = Pstr_eval (params, []); _ }])
        ->
        (name, Some params)
      | Ppat_extension (name, PStr []) -> (name, None)
      | _ -> invalid_input ~loc:binding.pvb_loc in
    let params =
      match params with
      | Some { pexp_desc = Pexp_tuple params; _ } -> params
      | Some param -> [param]
      | None -> [] in
    let params =
      List.map
        (fun param ->
          match param.pexp_desc with
          | Pexp_ident { txt = Lident name; loc } ->
            Location.{ txt = name; loc }
          | _ -> invalid_input ~loc:param.pexp_loc)
        params in
    let template = binding.pvb_expr in
    (* TODO: unused args *)
    (name, params, template)

  let extract_letextension ~loc payload =
    let let_ =
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (let_, []); _ }] -> let_
      | _ -> invalid_input ~loc in
    let binding, body =
      match let_.pexp_desc with
      | Pexp_let (Nonrecursive, [binding], body) -> (binding, body)
      | _ -> invalid_input ~loc:let_.pexp_loc in
    (extract_binding binding, body)

  let make_declaration_item ~loc ~name ~params ~template =
    let open Ast_helper in
    let type_name = Location.{ name with txt = wrap_type_name name.txt } in
    (* TODO: maybe Obj.magic on an additional field on attribute *)
    let attr =
      let loc = template.pexp_loc in
      let attr_name = Attribute_encoding.name ~loc in
      let attr_payload = Attribute_encoding.encode ~loc ~params ~template in
      { attr_name; attr_loc = loc; attr_payload } in
    let declaration = Type.mk ~loc ~attrs:[attr] type_name in
    Str.type_ ~loc Nonrecursive [declaration]

  (* TODO: any good usage for expected type? *)
  let expression ~loc ~env payload ~expected:_ =
    let (name, params, template), body = extract_letextension ~loc payload in
    let declaration_item = make_declaration_item ~loc ~name ~params ~template in

    let open Ast_helper in
    let module_wrapper_name = "*MACRO" in
    let opened_body =
      let open_ =
        Opn.mk ~loc (Mod.ident ~loc { txt = Lident module_wrapper_name; loc })
      in
      Exp.open_ ~loc open_ body in

    let module_wrapper =
      let module_ = Mod.structure ~loc [declaration_item] in
      Exp.letmodule ~loc
        { txt = Some module_wrapper_name; loc }
        module_ opened_body in
    Typecore.type_exp env module_wrapper

  let extract_structure_item ~loc payload =
    let binding =
      match payload with
      | PStr [{ pstr_desc = Pstr_value (Nonrecursive, [binding]); _ }] ->
        binding
      | _ -> invalid_input ~loc in
    extract_binding binding
  let structure_item ~loc ~env:_ payload k =
    let name, params, template = extract_structure_item ~loc payload in
    let stri = make_declaration_item ~loc ~name ~params ~template in
    k stri
end

(* let hooks = { Hooks.default with } *)
let () =
  let define_expr =
    Extension.declare "define" Extension.Context.expression
      Define_macro.expression in
  let define_stri =
    Extension.declare "define" Extension.Context.structure_item
      Define_macro.structure_item in
  register
    ~rules:
      [
        Context_free.Rule.extension define_expr;
        Context_free.Rule.extension define_stri;
      ]
    ~hooks:Unhandled_extension.hooks "ppx_macro"
