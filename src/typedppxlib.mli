open Typedppxlib_ocaml_typing
module Ast_pattern : sig
  type ('a, 'b, 'c) t

  val __ : ('a, 'a -> 'b, 'b) t
end
module Extension : sig
  module Context : sig
    type ('return, 'expected) t
    val core_type : (Typedtree.core_type, unit) t
    val expression : (Typedtree.expression, Typecore.type_expected) t
    val structure_item : (Typedtree.structure_item * Types.signature, unit) t
  end
  type t

  val declare :
    string ->
    ('return, 'expected) Context.t ->
    (Parsetree.payload, 'a, 'return) Ast_pattern.t ->
    (loc:Location.t -> env:Env.t -> expected:'expected -> 'a) ->
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

  val default : t
end

val register :
  ?rules:Context_free.Rule.t list ->
  ?hooks:Hooks.t ->
  ?impl:(Typedtree.structure -> Typedtree.structure) ->
  string ->
  unit
(** [register ~rules ~hooks ~impl name] *)
