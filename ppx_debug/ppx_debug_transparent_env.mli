open Typedppxlib_ocaml_typing

type t

module String : sig
  include module type of String
  module Map : Map.S with type key = string
end

type term = private
  | Type   of {
      rec_flag : Asttypes.rec_flag;
      type_ : Types.type_declaration;
      types : Types.type_declaration list;
    }
  (* TODO: I believe to be okay to use a single string per case,
     as shadowable names are never acessed by name, always by Ident *)
  | Module of (* TODO: what about mb_presence, mb_attributes?*)
              term String.Map.t

val lookup : Path.t -> t -> term option

val empty : t
val structure_item : t -> Typedtree.structure_item -> t
val signature_item : t -> Typedtree.signature_item -> t