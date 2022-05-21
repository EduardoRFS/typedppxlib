open Typedppxlib_ocaml_parsing
open Typedppxlib_ocaml_typing

module Extension : sig
  module Context : sig
    type 'return t
    val core_type : Typedtree.core_type t
    val expression : (expected:Typecore.type_expected -> Typedtree.expression) t
    val structure_item :
      ((Parsetree.structure_item ->
       Typedtree.structure_item_desc * Types.signature * Shape.Map.t * Env.t) ->
      Typedtree.structure_item_desc * Types.signature * Shape.Map.t * Env.t)
      t
  end
  type t

  val declare :
    string ->
    'return Context.t ->
    (loc:Location.t -> env:Env.t -> Parsetree.payload -> 'return) ->
    t
end
module Context_free : sig
  module Rule : sig
    type t

    val extension : Extension.t -> t
  end
end
module Hooks : sig
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

  val default : t
end

val register :
  ?rules:Context_free.Rule.t list ->
  ?hooks:Hooks.t ->
  ?impl:(Typedtree.structure -> Typedtree.structure) ->
  string ->
  unit
(** [register ~rules ~hooks ~impl name] *)
