open Typedppxlib
open Typedppxlib_ocaml_typing
open Typedppxlib_ocaml_parsing
open Parsetree

let error_functor ~loc () =
  Location.raise_errorf ~loc "currently functor types are not supported"
(* let error_cmi_not_found ~loc ident () =
   Location.raise_errorf ~loc "cmi for module was not found %a" Ident.print ident *)

(* TODO: figure out better solution for still not inferred types *)

let debug ~loc ~env payload ~expected:_ =
  match payload with
  | Parsetree.PStr [{ pstr_desc = Pstr_eval (expr, _); _ }] ->
    (* TODO: is may_forget_scope safe here? *)
    (* TODO: warning about non printable types *)
    let type_expr =
      let expr = Typecore.type_exp env expr in
      Ctype.(full_expand ~may_forget_scope:true env (instance expr.exp_type))
    in
    let id, runtime_data =
      let runtime_data = Ppx_debug_runtime_data.make ~env ~type_expr in
      let id = Ppx_debug_runtime_data.id runtime_data in
      let id = Ppx_debug_runtime_data.Id.to_string id in
      ( id,
        Ast_helper.(
          Const.string ~loc (Ppx_debug_runtime_data.to_string runtime_data)
          |> Exp.constant ~loc) ) in
    (* TODO: any advantage of using the ~expected? *)
    (* TODO: is this an stable output? Could we do better? *)
    let expr =
      [%expr
        Ppx_debug_runtime.truly_unsafe_print
          ~runtime_data:
            (let module M = struct
               external magic : 'a -> 'b = "%identity"
             end in
            (M.magic [%e runtime_data] : Ppx_debug_runtime.runtime_data))
          [%e expr]] in

    let ppx_debug_attribute =
      {
        attr_name = { txt = "ppx_debug." ^ id; loc };
        attr_payload = PStr [];
        attr_loc = loc;
      } in
    let expr = { expr with pexp_attributes = [ppx_debug_attribute] } in
    expr |> Typecore.type_exp env
  | _ -> Location.raise_errorf ~loc "expected a single expression"

let () =
  let extension =
    Extension.declare "debug" Extension.Context.expression debug
  in
  register ~rules:[Context_free.Rule.extension extension] "ppx_debug"
