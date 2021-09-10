[@@@warning "-33"]
(* TODO: fix the -33 error*)

open Base
open Float
open Nativeint
open Int32
open Int64

module String = struct
  include String
  let ( + ) a b = a ^ b
end
open String

let a = 1 + 2
let b = 1.0 + 2.0
let c = 1n + 1n
let d = 1l + 1l
let e = 1L + 1L
let f = "a" + "b"
