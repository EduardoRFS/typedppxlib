open Typedppxlib_ocaml_typing
module Hooks : sig
  type type_package =
    Env.t ->
    Parsetree.module_expr ->
    Path.t ->
    Longident.t list ->
    Typedtree.module_expr * Types.type_expr list
  type base = { type_package : type_package }

  type t = { type_package : base -> type_package }

  val default : t
end

val register :
  ?hooks:Hooks.t ->
  ?impl:(Typedtree.structure -> Typedtree.structure) ->
  string ->
  unit
(** [register name hook] *)
