let id x = x

type 'a t =
  | Leaf of 'a
  | Node of 'a node
and 'a node = 'a t * 'a t

module Transparent = struct
  type 'a t = int * 'a
  let x : 'a t option = Some (11, 12)
end
module Record = struct
  type 'a t = {
    a : int;
    b : 'a;
  }
  let x = { a = 16; b = 17.0 }
end
module Variant = struct
  type 'a t = A of float * int
  let x = A (14.0, 15)
end
module Opaque : sig
  type t
  val x : t
end = struct
  type t = { opaque : bool }
  let x = { opaque = true }
end
module Double_opaque : sig
  type t
  val x : t
end = struct
  type t = Double_opaque of Opaque.t
  let x = Double_opaque Opaque.x
end
module Existential = struct
  type t = Ex : 'a -> t
  let x = Ex 1
end
module Recursive = struct
  type 'a t =
    | One  of 'a
    | Cons of 'a * 'a t

  let x = Cons (1, Cons (2, One 3))
end

module Mutually_recursive = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a node
  and 'a node = Pair of 'a t * 'a t

  let x = Node (Pair (Node (Pair (Leaf 1, Leaf 2)), Leaf 3))
end

(* TODO: GADT *)
module Code = struct
  open Parsetree
  let loc = Location.none
  let code = [%str let x = 1 + 2]
  let tcode =
    let env =
      Compmisc.init_path ();
      Compmisc.initial_env () in
    Typemod.type_structure env code
end
let () =
  let lazy_value = lazy 18 in

  [%debug
    id,
      1,
      'c',
      "s",
      Bytes.of_string "b",
      2.0,
      true,
      (),
      Not_found,
      [|3.0; 4.0|],
      [(5, 6); (7, 8)],
      Some 5.0,
      6n,
      7l,
      8L,
      lazy (9 + 10),
      lazy_value,
      Opaque.x,
      Double_opaque.x,
      Transparent.x,
      Record.x,
      Variant.x,
      Existential.x,
      Recursive.x,
      Mutually_recursive.x,
      Code.code
    (* TODO: , Code.tcode *)]
