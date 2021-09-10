open Ppxlib
open Typedppxlib
open Typedppxlib_ocaml_typing

let expr_is_error expr =
  match expr.Typedtree.exp_attributes with
  | [ { attr_name = { txt = "**typedppxlib.recover**"; _ }; _ } ] -> true
  | _ -> false

let type_expect (super : Hooks.base) ?in_function ?recarg env expr expected =
  let type_exp expr =
    super.type_expect ?in_function ?recarg env expr expected
  in
  match expr.pexp_desc with
  | Pexp_apply
      ( ({
           pexp_desc =
             Pexp_ident { txt = Lident expected_name; loc = lident_loc };
           _;
         } as expr_ident),
        args ) ->
      (* TODO: this never fails because of error recovery *)
      let original_result = type_exp expr in
      if expr_is_error original_result then
        (* TODO: this is really inneficient *)
        Env.fold_values
          (fun name path _vd acc ->
            match acc with
            | Some _ -> acc
            | None when name = expected_name ->
                let lident = Untypeast.lident_of_path path in
                let left =
                  {
                    expr_ident with
                    pexp_desc = Pexp_ident { txt = lident; loc = lident_loc };
                  }
                in
                let expr = { expr with pexp_desc = Pexp_apply (left, args) } in
                let result = type_exp expr in
                if expr_is_error result then None else Some result
            | None ->
                acc)
          None env None
        |> Option.value ~default:original_result
      else original_result
  | _ -> type_exp expr

let () =
  let hooks = { Hooks.default with type_expect } in
  register ~hooks "ppx_overload"
