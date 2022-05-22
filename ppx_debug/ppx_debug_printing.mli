module Printing_ir : sig
  type t
  val to_string : t -> string
end
module Translate_typ : sig
  open Typedppxlib_ocaml_typing
  val translate_typ :
    Env.t -> Ppx_debug_transparent_env.t -> Types.type_expr -> Printing_ir.t
end

type runtime_data
type partial_ir
val truly_unsafe_pp :
  Format.formatter ->
  runtime_data:runtime_data ->
  partial_ir:partial_ir ->
  'a ->
  unit
