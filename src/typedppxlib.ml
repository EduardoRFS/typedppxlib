module Hooks = struct
  open Typedppxlib_ocaml_typing

  type type_package =
    Env.t ->
    Parsetree.module_expr ->
    Path.t ->
    Longident.t list ->
    Typedtree.module_expr * Types.type_expr list
  type type_expect =
    ?in_function:Warnings.loc * Types.type_expr ->
    ?recarg:Typecore.recarg ->
    Env.t ->
    Parsetree.expression ->
    Typecore.type_expected ->
    Typedtree.expression
  type type_extension =
    ?in_function:Warnings.loc * Types.type_expr ->
    recarg:Typecore.recarg ->
    Env.t ->
    Parsetree.expression ->
    Typecore.type_expected ->
    Parsetree.extension ->
    Typedtree.expression

  type base = {
    type_package : type_package;
    type_expect : type_expect;
    type_extension : type_extension;
  }

  type t = {
    type_package : base -> type_package;
    type_expect : base -> type_expect;
    type_extension : base -> type_extension;
  }

  let default =
    {
      type_package = (fun super -> super.type_package);
      type_expect = (fun super -> super.type_expect);
      type_extension = (fun super -> super.type_extension);
    }

  let instance =
    ref
      ({
         type_package = !Typecore.type_package;
         type_expect = !Typecore.type_expect_ref;
         type_extension = !Typecore.type_extension_ref;
       }
        : base)
  let register hook =
    let super = !instance in

    instance :=
      {
        type_package = (fun env -> hook.type_package super env);
        type_expect = (fun ?in_function -> hook.type_expect super ?in_function);
        type_extension =
          (fun ?in_function -> hook.type_extension super ?in_function);
      }

  (* register hooks *)
  let () = Typecore.type_package := fun env -> !instance.type_package env
  let () =
    Typecore.type_expect_ref :=
      fun ?in_function -> !instance.type_expect ?in_function
  let () =
    Typecore.type_extension_ref :=
      fun ?in_function -> !instance.type_extension ?in_function
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
