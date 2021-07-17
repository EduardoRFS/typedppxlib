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
module Ast_pattern = struct
  type ('a, 'b, 'c) t = To_b : ('a, 'a -> 'b, 'b) t
  let __ = To_b
end
module Extension = struct
  open Typedppxlib_ocaml_typing

  module Context = struct
    type ('return, 'expected) t =
      | Expression : (Typedtree.expression, Typecore.type_expected) t
    let expression = Expression
  end
  type t =
    | T : {
        name : string;
        context : ('return, 'expected) Context.t;
        pattern : (Parsetree.payload, 'a, 'return) Ast_pattern.t;
        expander : loc:Location.t -> env:Env.t -> expected:'expected -> 'a;
      }
        -> t
  let declare name context pattern expander =
    T { name; context; pattern; expander }

  let instance = Hashtbl.create 8
  let register (T extension) =
    match Hashtbl.find_opt instance extension.name with
    (* what to do here? *)
    | Some _ ->
        failwith (Printf.sprintf "two ppx with same name %s" extension.name)
    | None -> Hashtbl.add instance extension.name (T extension)

  let () =
    let hooks =
      {
        Hooks.default with
        type_extension =
          (fun super ?in_function ~recarg env expr expected
               ((name, payload) as extension) ->
            let Location.{ txt = name; loc = name_loc } = name in
            match Hashtbl.find_opt instance name with
            (* TODO: which loc goes here *)
            | Some
                (T ({ context = Expression; pattern = To_b; _ } as extension))
              ->
                extension.expander ~loc:name_loc ~env ~expected payload
            | None ->
                super.type_extension ?in_function ~recarg env expr expected
                  extension);
      }
    in
    Hooks.register hooks
end
module Context_free = struct
  module Rule = struct
    type t = Extension.t
    let extension t = t
  end
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

let registered = ref false

let register ?(rules = []) ?hooks ?impl _name =
  if not !registered then (
    let open Ppxlib in
    registered := true;
    Driver.register_transformation
      ~instrument:(Driver.Instrument.make ~position:After Transform.transform)
      "typedppxlib");
  List.iter Extension.register rules;
  (match hooks with Some hooks -> Hooks.register hooks | None -> ());
  match impl with Some impl -> Transform.register impl | None -> ()
