open Typedppxlib_ocaml_typing
open Typedtree

module String = struct
  include String
  module Map = Map.Make (String)
end

let ( let* ) = Option.bind
let bug () = failwith "this is likely a bug"

(* let unimplemented () = failwith "unimplemented" *)
let unimplemented_none () = None
let unimplemented_env env = env

(* TODO: why not use OCaml Env? *)
type term =
  | Type   of {
      rec_flag : Asttypes.rec_flag;
      type_ : Types.type_declaration;
      types : Types.type_declaration list;
    }
  (* TODO: I believe to be okay to use a single string per case,
     as shadowable names are never acessed by name, always by Ident *)
  | Module of (* TODO: what about mb_presence, mb_attributes?*)
              term String.Map.t

type t = {
  idents : term Ident.tbl;
  (* TODO: Hashtbl? *)
  current_module : term String.Map.t;
  module_stack : term String.Map.t list;
}
let empty =
  { idents = Ident.empty; current_module = String.Map.empty; module_stack = [] }

let find_same_opt ident tbl =
  try Some (Ident.find_same ident tbl) with
  | Not_found -> None
let mem ident tbl = Option.is_some (find_same_opt ident tbl)
let rec lookup path env =
  match path with
  | Path.Pident ident -> find_same_opt ident env.idents
  | Path.Pdot (left, right) ->
    let* fields = lookup_module left env in
    String.Map.find_opt right fields
  | Path.Papply _ -> unimplemented_none ()

and lookup_type path env =
  let* term = lookup path env in
  match term with
  | Type { rec_flag = _; type_; types = _ } -> Some type_
  | Module _ -> (* TODO: maybe bug? *) None

and lookup_module path env =
  let* term = lookup path env in
  match term with
  | Type _ -> (* TODO: maybe bug? *) None
  | Module fields -> Some fields

let enter ident term env =
  let { idents; current_module; module_stack } = env in
  (* TODO: what if Ident is defined twice *)
  if mem ident idents then
    bug ();

  let name = Ident.name ident in
  let idents = Ident.add ident term idents in
  let current_module = String.Map.add name term current_module in
  { idents; current_module; module_stack }

let enter_module env =
  let { idents; current_module; module_stack } = env in
  let module_stack = current_module :: module_stack in
  let current_module = String.Map.empty in
  { idents; current_module; module_stack }
let leave_module env =
  let { idents; current_module = leaving_module; module_stack } = env in
  match module_stack with
  | current_module :: module_stack ->
    let env = { idents; current_module; module_stack } in
    (leaving_module, env)
  | [] -> bug ()

let rec structure env str =
  let env = enter_module env in
  let env = List.fold_left structure_item env str.str_items in
  leave_module env

and structure_item env stri =
  match stri.str_desc with
  | Tstr_eval _ -> env
  | Tstr_value _ -> env
  | Tstr_primitive _ -> env
  | Tstr_type (rec_flag, decls) -> type_declarations env rec_flag decls
  | Tstr_typext _ -> unimplemented_env env
  | Tstr_exception _ -> unimplemented_env env
  | Tstr_module module_ -> module_binding env module_
  | Tstr_recmodule _ -> unimplemented_env env
  | Tstr_include _ -> unimplemented_env env
  | Tstr_modtype _
  | Tstr_open _
  | Tstr_class _
  | Tstr_class_type _
  | Tstr_attribute _ ->
    env

(* TODO: signature?? *)
and signature env sig_ =
  let env = enter_module env in
  let env = List.fold_left signature_item env sig_.sig_items in
  leave_module env

and signature_item env sigi =
  match sigi.sig_desc with
  | Tsig_type (rec_flag, decls) -> type_declarations env rec_flag decls
  | Tsig_typesubst _ -> unimplemented_env env
  | Tsig_typext _ -> unimplemented_env env
  | Tsig_exception _ -> unimplemented_env env
  | Tsig_module module_ -> module_declaration env module_
  | Tsig_modsubst _ -> unimplemented_env env
  | Tsig_recmodule _ -> unimplemented_env env
  | Tsig_include _ -> unimplemented_env env
  | Tsig_value _ -> env
  | Tsig_modtype _ -> env
  | Tsig_modtypesubst _ -> env
  | Tsig_open _ -> env
  | Tsig_class _ -> env
  | Tsig_class_type _ -> env
  | Tsig_attribute _ -> env

and type_declarations env rec_flag decls =
  let decls = List.map (fun decl -> (decl.typ_id, decl.typ_type)) decls in
  let types = List.map snd decls in
  List.fold_left
    (fun env (ident, type_) ->
      let term = Type { rec_flag; type_; types } in
      enter ident term env)
    env decls

and module_binding env module_ =
  match module_.mb_id with
  | Some ident -> (
    match module_expr env module_.mb_expr with
    | Some (fields, env) -> enter ident (Module fields) env
    | None -> env)
  | None -> env

and module_expr env module_ =
  match module_.mod_desc with
  | Tmod_ident (path, _lid) ->
    let* fields = lookup_module path env in
    Some (fields, env)
  | Tmod_structure str ->
    let fields, env = structure env str in
    Some (fields, env)
  | Tmod_functor _ -> unimplemented_none ()
  | Tmod_apply _ -> unimplemented_none ()
  | Tmod_constraint (internal_module, _mty, _constraint, _coercion) ->
    module_expr env internal_module
  | Tmod_unpack _ -> unimplemented_none ()

and module_declaration env module_ =
  match module_.md_id with
  | Some ident -> (
    match module_type env module_.md_type with
    | Some (fields, env) -> enter ident (Module fields) env
    | None -> env)
  | None -> env

and module_type env mty =
  match mty.mty_desc with
  | Tmty_ident (path, _lid)
  | Tmty_alias (path, _lid) ->
    let* fields = lookup_module path env in
    Some (fields, env)
  | Tmty_signature sig_ ->
    let fields, env = signature env sig_ in
    Some (fields, env)
  | Tmty_functor _ -> unimplemented_none ()
  | Tmty_with _ -> unimplemented_none ()
  | Tmty_typeof _ -> unimplemented_none ()
