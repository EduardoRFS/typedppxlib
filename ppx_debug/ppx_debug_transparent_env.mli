open Typedppxlib_ocaml_typing

type t

val lookup_type : Path.t -> t -> Types.type_declaration option

val empty : t
val structure_item : t -> Typedtree.structure_item -> t
val signature_item : t -> Typedtree.signature_item -> t
