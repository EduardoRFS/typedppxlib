module Hooks = struct
  open Ocaml_common
  type type_package =
    Env.t ->
    Parsetree.module_expr ->
    Path.t ->
    Longident.t list ->
    Typedtree.module_expr * Types.type_expr list
  type base = { type_package' : type_package }

  type t = { type_package : base -> type_package }

  let default = { type_package = (fun super -> super.type_package') }

  let instance = ref { type_package' = (fun env -> !Typecore.type_package env) }
  let register hook =
    let super = !instance in

    instance := { type_package' = (fun env -> hook.type_package super env) }
end

open Ppxlib
open Ocaml_common

let registered = ref false

let env =
  lazy
    (Compmisc.init_path ();
     Compmisc.initial_env ())
let transform str =
  let env = Lazy.force_val env in
  let str = Ppxlib_ast.Selected_ast.to_ocaml Structure str in
  let tstr, _, _, _ = Typemod.type_structure env str in
  Untypeast.untype_structure tstr |> Ppxlib_ast.Selected_ast.of_ocaml Structure
let register _name hook =
  if not !registered then (
    registered := true;
    Driver.register_transformation
      ~instrument:(Driver.Instrument.make ~position:After transform)
      "typedppxlib");
  Hooks.register hook