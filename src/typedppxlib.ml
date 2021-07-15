module Hooks = struct
  open Typedppxlib_ocaml_typing
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

module Transform = struct
  open Typedppxlib_ocaml_driver
  open Typedppxlib_ocaml_typing

  let env =
    lazy
      (Compmisc.init_path ();
       Compmisc.initial_env ())

  let instance = ref (fun tstr -> tstr)
  let register transform =
    let super = !instance in
    instance := fun tstr -> super (transform tstr)

  let transform str =
    let env = Lazy.force_val env in
    let tstr, _, _, _ = Typemod.type_structure env str in
    let transform = !instance in
    Untypeast.untype_structure (transform tstr)
end
open Ppxlib

let registered = ref false

let register ?hooks ?impl _name =
  if not !registered then (
    registered := true;
    Driver.register_transformation
      ~instrument:(Driver.Instrument.make ~position:After Transform.transform)
      "typedppxlib");
  (match hooks with Some hooks -> Hooks.register hooks | None -> ());
  match impl with Some impl -> Transform.register impl | None -> ()
