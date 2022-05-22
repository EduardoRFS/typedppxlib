open Typedppxlib
open Typedppxlib_ocaml_typing
open Typedppxlib_ocaml_parsing
open Parsetree

let () = Printexc.record_backtrace true

let error_functor ~loc () =
  Location.raise_errorf ~loc "currently functor types are not supported"
(* let error_cmi_not_found ~loc ident () =
   Location.raise_errorf ~loc "cmi for module was not found %a" Ident.print ident *)

(* TODO: figure out better solution for still not inferred types *)
(* TODO: maybe lazy shadow env? *)
module Shadow_stack = struct
  (* TODO: maybe make it non global? Maybe domain based? *)
  let current_stack : Typedtree.structure_item list list ref = ref [[]]
  let reset () = current_stack := [[]]
  let current () =
    match !current_stack with
    | current :: _rest -> current
    | [] -> assert false
  let rest () =
    match !current_stack with
    | _current :: rest -> rest
    | [] -> assert false

  let enter_module () = current_stack := [] :: !current_stack
  let leave_module () = current_stack := rest ()

  let enter_item stri =
    let current = current () in
    let current = stri :: current in
    current_stack := current :: rest ()

  let build_env () =
    let open Ppx_debug_transparent_env in
    let str = List.rev_map List.rev !current_stack |> List.flatten in
    List.fold_left structure_item empty str
end

let type_structure (super : Hooks.base) ~toplevel funct_body anchor env str =
  let () = Shadow_stack.enter_module () in
  let str, sig_, names, shape, env =
    super.type_structure ~toplevel funct_body anchor env str in
  let () = Shadow_stack.leave_module () in
  (str, sig_, names, shape, env)

let type_str_item (super : Hooks.base) ~toplevel funct_body anchor env shape_map
    stri =
  let desc, sg, shape_map, new_env =
    super.type_str_item ~toplevel funct_body anchor env shape_map stri in
  let () =
    let stri =
      Typedtree.{ str_desc = desc; str_loc = stri.pstr_loc; str_env = env }
    in
    Shadow_stack.enter_item stri in
  (desc, sg, shape_map, new_env)

let hooks = { Hooks.default with type_structure; type_str_item }

type return_kind =
  | Unit
  | Id
  | PP
let extract ~loc payload =
  match payload with
  | Parsetree.PStr [{ pstr_desc = Pstr_eval (expr, _); _ }] -> expr
  | _ -> Location.raise_errorf ~loc "expected a single expression"

let debug kind ~loc ~env payload ~expected:_ =
  let expr = extract ~loc payload in
  (* TODO: is may_forget_scope safe here? *)
  (* TODO: warning about non printable types *)
  let type_expr =
    let expr = Typecore.type_exp env expr in
    Ctype.(full_expand ~may_forget_scope:true env (instance expr.exp_type))
  in
  let const_string string =
    Ast_helper.(Const.string ~loc string |> Exp.constant ~loc) in
  let id, runtime_data =
    let runtime_data = Ppx_debug_runtime.make ~env in
    let id = Ppx_debug_runtime.(Id.to_string (id runtime_data)) in
    (id, const_string (Ppx_debug_runtime.to_string runtime_data)) in
  let transparent_env =
    try Shadow_stack.build_env () with
    | exn ->
      Printexc.print_backtrace stderr;
      raise exn in
  (* TODO: any advantage of using the ~expected? *)
  (* TODO: is this an stable output? Could we do better? *)
  (* TODO: ir could also be done lazily *)
  let partial_ir =
    let open Ppx_debug_printing in
    let partial_ir =
      try Translate_typ.translate_typ env transparent_env type_expr with
      | exn ->
        Printexc.print_backtrace stderr;
        raise exn in
    const_string (Printing_ir.to_string partial_ir) in

  let pp_expr x =
    [%expr
      fun fmt () ->
        Ppx_debug_printing.truly_unsafe_pp fmt
          ~runtime_data:
            (let module M = struct
               external magic : 'a -> 'b = "%identity"
             end in
            (M.magic [%e runtime_data] : Ppx_debug_printing.runtime_data))
          ~partial_ir:
            (let module M = struct
               external magic : 'a -> 'b = "%identity"
             end in
            (M.magic [%e partial_ir] : Ppx_debug_printing.partial_ir))
          [%e x]] in
  let expr =
    match kind with
    | Unit -> [%expr Format.eprintf "%a\n%!" [%e pp_expr expr] ()]
    | Id ->
      [%expr
        let x = [%e expr] in
        Format.eprintf "%a\n%!" [%e pp_expr [%expr x]] ();
        x]
    | PP -> pp_expr expr in

  let ppx_debug_attribute =
    {
      attr_name = { txt = "ppx_debug." ^ id; loc };
      attr_payload = PStr [];
      attr_loc = loc;
    } in
  let expr = { expr with pexp_attributes = [ppx_debug_attribute] } in
  expr |> Typecore.type_exp env

let () =
  let debug_unit =
    Extension.declare "debug" Extension.Context.expression (debug Unit) in
  let debug_id =
    Extension.declare "debug.id" Extension.Context.expression (debug Id) in
  let debug_pp =
    Extension.declare "debug.pp" Extension.Context.expression (debug PP) in
  register
    ~rules:
      [
        Context_free.Rule.extension debug_unit;
        Context_free.Rule.extension debug_id;
        Context_free.Rule.extension debug_pp;
      ]
    ~hooks "ppx_debug"
