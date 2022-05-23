module Hooks = struct
  open Typedppxlib_ocaml_parsing
  open Typedppxlib_ocaml_typing

  type type_package =
    Env.t ->
    Parsetree.module_expr ->
    Path.t ->
    (Longident.t * Types.type_expr) list ->
    Typedtree.module_expr * (Longident.t * Types.type_expr) list
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
  type type_structure =
    toplevel:bool ->
    bool ->
    Path.t option ->
    Env.t ->
    Parsetree.structure ->
    Typedtree.structure
    * Types.signature_item list
    * Typemod.Signature_names.t
    * Shape.t
    * Env.t
  type type_str_item =
    toplevel:bool ->
    bool ->
    Path.t option ->
    Env.t ->
    Shape.Map.t ->
    Parsetree.structure_item ->
    Typedtree.structure_item_desc * Types.signature * Shape.Map.t * Env.t
  type read_cmi = string -> Cmi_format.cmi_infos

  type base = {
    type_package : type_package;
    type_expect : type_expect;
    type_extension : type_extension;
    transl_type : transl_type;
    transl_extension : transl_extension;
    type_structure : type_structure;
    type_str_item : type_str_item;
    read_cmi : read_cmi;
  }

  type t = {
    type_package : base -> type_package;
    type_expect : base -> type_expect;
    type_extension : base -> type_extension;
    transl_type : base -> transl_type;
    transl_extension : base -> transl_extension;
    type_structure : base -> type_structure;
    type_str_item : base -> type_str_item;
    read_cmi : base -> read_cmi;
  }

  let default =
    {
      type_package = (fun super -> super.type_package);
      type_expect = (fun super -> super.type_expect);
      type_extension = (fun super -> super.type_extension);
      transl_type = (fun super -> super.transl_type);
      transl_extension = (fun super -> super.transl_extension);
      type_structure = (fun super -> super.type_structure);
      type_str_item = (fun super -> super.type_str_item);
      read_cmi = (fun super -> super.read_cmi);
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
         (* TODO: that's not great because we ignore this parameters *)
         type_structure = !Typemod.type_structure_ref;
         type_str_item = (fun ~toplevel:_ _ _ -> !type_str_item_source);
         read_cmi = !Cmi_format.read_cmi_ref;
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
        type_structure = (fun ~toplevel -> hook.type_structure super ~toplevel);
        type_str_item = (fun ~toplevel -> hook.type_str_item super ~toplevel);
        read_cmi = (fun filename -> hook.read_cmi super filename);
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
    (Typemod.type_structure_ref :=
       fun ~toplevel -> !instance.type_structure ~toplevel);
    Typemod.type_str_item_ref :=
      fun type_str_item ->
        type_str_item_source := type_str_item;
        !instance.type_str_item
  let () =
    Cmi_format.read_cmi_ref := fun filename -> !instance.read_cmi filename
end

module Extension = struct
  open Typedppxlib_ocaml_parsing
  open Typedppxlib_ocaml_typing

  type ('a, 'b) eq = Eq : ('a, 'a) eq
  module Context = struct
    type 'return t =
      | Core_type : Typedtree.core_type t
      | Expression : (expected:Typecore.type_expected -> Typedtree.expression) t
      (* TODO: this API is clearly weird *)
      | Structure_item
          : ((Parsetree.structure_item ->
             Typedtree.structure_item_desc
             * Types.signature
             * Shape.Map.t
             * Env.t) ->
            Typedtree.structure_item_desc
            * Types.signature
            * Shape.Map.t
            * Env.t)
            t
    let core_type = Core_type
    let expression = Expression
    let structure_item = Structure_item

    let is_equal : type a b. a t -> b t -> (a, b) eq option =
     fun a b ->
      match (a, b) with
      | Core_type, Core_type -> Some Eq
      | Expression, Expression -> Some Eq
      | Structure_item, Structure_item -> Some Eq
      | _ -> None
  end
  type 'return expander =
    loc:Location.t -> env:Env.t -> Parsetree.payload -> 'return
  type t =
    | T : {
        name : string;
        context : 'return Context.t;
        expander : 'return expander;
      }
        -> t
  let declare name context expander = T { name; context; expander }

  let instance = Hashtbl.create 8
  let find_expander_by_context (type a) name (expected_context : a Context.t) =
    let cases = Hashtbl.find_all instance name in
    List.find_map
      (fun (T { context; expander; _ }) ->
        match Context.is_equal expected_context context with
        | Some Eq -> Some (expander : a expander)
        | None -> None)
      cases
  let register (T { name; context; expander = _ } as extension) =
    match find_expander_by_context name context with
    (* what to do here? *)
    | Some _ -> failwith (Printf.sprintf "two ppx with same name %s" name)
    | None -> Hashtbl.add instance name extension

  let () =
    let find_expander : type a. a Context.t -> string -> a expander option =
     fun expected_context name -> find_expander_by_context name expected_context
    in

    let hooks =
      {
        Hooks.default with
        type_extension =
          (fun super ?in_function ~recarg env expr expected
               (({ txt = name; loc = name_loc }, payload) as extension) ->
            match find_expander Expression name with
            (* TODO: which loc goes here *)
            | Some expander -> expander ~loc:name_loc ~env ~expected payload
            | None ->
                super.type_extension ?in_function ~recarg env expr expected
                  extension);
        transl_extension =
          (fun super env policy styp
               (({ txt = name; loc = name_loc }, payload) as extension) ->
            match find_expander Core_type name with
            (* TODO: which loc goes here *)
            | Some expander -> expander ~loc:name_loc ~env payload
            | None -> super.transl_extension env policy styp extension);
        type_str_item =
          (fun super ~toplevel funct_body anchor env shape_map stri ->
            match stri.pstr_desc with
            | Pstr_extension (({ txt = name; loc = name_loc }, payload), _attrs)
              -> (
                match find_expander Structure_item name with
                (* TODO: which loc goes here *)
                | Some expander ->
                    expander ~loc:name_loc ~env payload (fun stri ->
                        super.type_str_item ~toplevel funct_body anchor env
                          shape_map stri)
                    (* TODO: preserve the loc by the user *)
                | None ->
                    super.type_str_item ~toplevel funct_body anchor env
                      shape_map stri)
            | _ ->
                super.type_str_item ~toplevel funct_body anchor env shape_map
                  stri);
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
module Error_recovery = struct
  open Typedppxlib_ocaml_parsing
  open Typedppxlib_ocaml_typing
  let snapshot () =
    let btype_snapshot = Btype.snapshot () in
    let ctype_levels = Ctype.save_levels () in
    fun () ->
      Btype.backtrack btype_snapshot;
      Ctype.set_levels ctype_levels
  let recover_attr stri =
    Parsetree.
      {
        attr_name = { txt = "**typedppxlib.recover**"; loc = Location.none };
        attr_payload = PStr [ stri ];
        attr_loc = Location.none;
      }
  let () =
    let hooks =
      {
        Hooks.default with
        type_expect =
          (fun super ?in_function ?recarg env expr ty_expected ->
            let reset = snapshot () in
            try super.type_expect ?in_function ?recarg env expr ty_expected
            with _exn ->
              (* TODO: use this exception to something *)
              reset ();
              {
                exp_desc = Texp_unreachable;
                exp_loc = Location.none;
                exp_extra = [];
                exp_type = Ctype.newty (Tunivar None);
                exp_attributes =
                  [
                    recover_attr
                      {
                        pstr_desc = Parsetree.Pstr_eval (expr, []);
                        pstr_loc = Location.none;
                      };
                  ];
                exp_env = env;
              });
        type_str_item =
          (fun super ~toplevel funct_body anchor env shape_map str ->
            let reset = snapshot () in
            try
              super.type_str_item ~toplevel funct_body anchor env shape_map str
            with _exn ->
              (* TODO: how to handle exceptions *)
              reset ();
              (Tstr_attribute (recover_attr str), [], shape_map, env));
      }
    in
    Hooks.register hooks

  let untype =
    let mapper =
      {
        Untypeast.default_mapper with
        expr =
          (fun sub expr ->
            match expr with
            | Typedtree.
                {
                  exp_attributes =
                    [
                      {
                        attr_name = { txt = "**typedppxlib.recover**"; _ };
                        attr_payload =
                          PStr
                            [
                              { pstr_desc = Pstr_eval (expr, _); pstr_loc = _ };
                            ];
                        attr_loc = _;
                      };
                    ];
                  exp_desc = Texp_unreachable;
                  _;
                } ->
                expr
            | _ -> Untypeast.default_mapper.expr sub expr);
        structure_item =
          (fun sub stri ->
            match stri.Typedtree.str_desc with
            | Tstr_attribute
                {
                  attr_name = { txt = "**typedppxlib.recover**"; _ };
                  attr_payload = PStr [ stri ];
                  attr_loc = _;
                } ->
                stri
            | _ -> Untypeast.default_mapper.structure_item sub stri);
      }
    in
    fun tstr -> mapper.structure mapper tstr
end

[%%if ocaml_version < (4, 14, 0)]
module Cmi_compatibility = struct
  open Typedppxlib_ocaml_typing

  (* TODO: this is copied from Cmi_format *)
  let read_cmi filename =
    let module Config = Ocaml_common.Config in
    let open Ocaml_common.Cmi_format in
    let ic = open_in_bin filename in
    try
      let buffer =
        really_input_string ic (String.length Config.cmi_magic_number)
      in
      if buffer <> Config.cmi_magic_number then (
        close_in ic;
        let pre_len = String.length Config.cmi_magic_number - 3 in
        if
          String.sub buffer 0 pre_len
          = String.sub Config.cmi_magic_number 0 pre_len
        then
          let msg =
            if buffer < Config.cmi_magic_number then "an older" else "a newer"
          in
          raise (Error (Wrong_version_interface (filename, msg)))
        else raise (Error (Not_an_interface filename)));
      let cmi = input_cmi ic in
      close_in ic;
      cmi
    with
    | End_of_file | Failure _ ->
        close_in ic;
        raise (Error (Corrupted_interface filename))
    | Error e ->
        close_in ic;
        raise (Error e)

  let read_cmi filename =
    let open Migrate_types in
    let cmi = read_cmi filename in
    let open struct
      let cmi_sign = cmi.cmi_sign
      [%%if ocaml_version < (4, 09, 0)]
      let cmi_sign = Migrate_408_409.copy_signature cmi_sign
      [%%endif]
      [%%if ocaml_version < (4, 10, 0)]
      let cmi_sign = Migrate_409_410.copy_signature cmi_sign
      [%%endif]
      [%%if ocaml_version < (4, 11, 0)]
      let cmi_sign = Migrate_410_411.copy_signature cmi_sign
      [%%endif]
      [%%if ocaml_version < (4, 12, 0)]
      let cmi_sign = Migrate_411_412.copy_signature cmi_sign
      [%%endif]
      [%%if ocaml_version < (4, 13, 0)]
      let cmi_sign = Migrate_412_413.copy_signature cmi_sign
      [%%endif]
      [%%if ocaml_version < (4, 14, 0)]
      let cmi_sign = Migrate_413_414.copy_signature cmi_sign
      [%%endif]
    end in
    (* TODO: check this on every update *)
    let cmi_sign : Types_414.Types.signature = cmi_sign in
    let cmi_sign : Types.signature = Obj.magic cmi_sign in
    (* TODO: cmi_flags is partially broken, as Unsafe_string was removed *)
    Cmi_format.
      {
        cmi_name = cmi.cmi_name;
        cmi_sign;
        cmi_crcs = cmi.cmi_crcs;
        (* TODO: check this on every update *)
        cmi_flags = Obj.magic cmi.cmi_flags;
      }
  let () = Hooks.register { Hooks.default with read_cmi = (fun _ -> read_cmi) }
end
[%%endif]

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
    let tstr, _, _, _, _ = Typemod.type_structure env str in
    let transform = !instance in
    Error_recovery.untype (transform tstr)
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
