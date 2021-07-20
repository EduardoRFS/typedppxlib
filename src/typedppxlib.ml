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
  type transl_type =
    Env.t -> Typetexp.policy -> Parsetree.core_type -> Typedtree.core_type
  type transl_extension =
    Env.t ->
    Typetexp.policy ->
    Parsetree.core_type ->
    Parsetree.extension ->
    Typedtree.core_type
  type type_str_item =
    toplevel:bool ->
    bool ->
    Path.t option ->
    Env.t ->
    Parsetree.structure_item ->
    Typedtree.structure_item_desc * Types.signature * Env.t

  type base = {
    type_package : type_package;
    type_expect : type_expect;
    type_extension : type_extension;
    transl_type : transl_type;
    transl_extension : transl_extension;
    type_str_item : type_str_item;
  }

  type t = {
    type_package : base -> type_package;
    type_expect : base -> type_expect;
    type_extension : base -> type_extension;
    transl_type : base -> transl_type;
    transl_extension : base -> transl_extension;
    type_str_item : base -> type_str_item;
  }

  let default =
    {
      type_package = (fun super -> super.type_package);
      type_expect = (fun super -> super.type_expect);
      type_extension = (fun super -> super.type_extension);
      transl_type = (fun super -> super.transl_type);
      transl_extension = (fun super -> super.transl_extension);
      type_str_item = (fun super -> super.type_str_item);
    }

  let type_str_item_source = ref (fun _ -> assert false)
  let instance =
    ref
      ({
         type_package = !Typecore.type_package;
         type_expect = !Typecore.type_expect_ref;
         type_extension = !Typecore.type_extension_ref;
         transl_type = !Typetexp.transl_type_ref;
         transl_extension = !Typetexp.transl_extension_ref;
         type_str_item = (fun ~toplevel:_ _ _ -> !type_str_item_source);
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
        transl_type = (fun env -> hook.transl_type super env);
        transl_extension = (fun env -> hook.transl_extension super env);
        type_str_item = (fun ~toplevel -> hook.type_str_item super ~toplevel);
      }

  (* register hooks *)
  let () = Typecore.type_package := fun env -> !instance.type_package env
  let () =
    Typecore.type_expect_ref :=
      fun ?in_function -> !instance.type_expect ?in_function
  let () =
    Typecore.type_extension_ref :=
      fun ?in_function -> !instance.type_extension ?in_function
  let () = Typetexp.transl_type_ref := fun env -> !instance.transl_type env
  let () =
    Typetexp.transl_extension_ref := fun env -> !instance.transl_extension env
  let () =
    Typemod.type_str_item_ref :=
      fun type_str_item ->
        type_str_item_source := type_str_item;
        !instance.type_str_item
end
module Ast_pattern = struct
  type ('a, 'b, 'c) t = To_b : ('a, 'a -> 'b, 'b) t
  let __ = To_b
end
module Extension = struct
  open Typedppxlib_ocaml_typing

  module Context = struct
    type ('return, 'expected) t =
      | Core_type : (Typedtree.core_type, unit) t
      | Expression : (Typedtree.expression, Typecore.type_expected) t
    let core_type = Core_type
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
               (({ txt = name; loc = name_loc }, payload) as extension) ->
            match Hashtbl.find_opt instance name with
            (* TODO: which loc goes here *)
            | Some
                (T ({ context = Expression; pattern = To_b; _ } as extension))
              ->
                extension.expander ~loc:name_loc ~env ~expected payload
            | Some _ | None ->
                super.type_extension ?in_function ~recarg env expr expected
                  extension);
        transl_extension =
          (fun super env policy styp
               (({ txt = name; loc = name_loc }, payload) as extension) ->
            match Hashtbl.find_opt instance name with
            (* TODO: which loc goes here *)
            | Some (T ({ context = Core_type; pattern = To_b; _ } as extension))
              ->
                extension.expander ~loc:name_loc ~env ~expected:() payload
            | Some _ | None -> super.transl_extension env policy styp extension);
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
