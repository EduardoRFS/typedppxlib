open Typedppxlib_ocaml_typing
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

  val default : t
end

val register :
  ?hooks:Hooks.t ->
  ?impl:(Typedtree.structure -> Typedtree.structure) ->
  string ->
  unit
(** [register name hook] *)
