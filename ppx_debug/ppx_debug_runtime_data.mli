open Typedppxlib_ocaml_typing

module Id : sig
  type t
  val equal : t -> t -> bool

  val unsafe_of_string : string -> t
  val to_string : t -> string
end

type t

val make : env:Env.t -> type_expr:Types.type_expr -> t
val id : t -> Id.t
val cwd : t -> string
val paths : t -> string list
val env : t -> Env.t
val type_expr : t -> Types.type_expr
val unsafe_of_string : string -> t
val to_string : t -> string
