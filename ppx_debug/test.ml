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
  type t = int
  let x = 13
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
      Transparent.x,
      Record.x,
      Variant.x,
      Existential.x,
      Recursive.x,
      Mutually_recursive.x]

(* TODO: print Typedtree *)
(* TODO: Base something*)
(* open Parsetree

   let () =
     let loc = Location.none in
     let str =
       [%str
         module M : sig
           type t
         end = struct
           type t = Int
         end] in
     let env = Env.empty in
     let tstr, _sig, _names, _final_env = Typemod.type_structure env str in
     [%debug _sig];
     let mod_binding =
       match (List.hd tstr.str_items).str_desc with
       | Tstr_module mod_binding -> mod_binding
       | _ -> failwith "invalid" in
     let internal, external_sig =
       match mod_binding.mb_expr.mod_desc with
       | Tmod_constraint (md, mty, _constraint, _coercion) -> (md, mty)
       | _ -> failwith "invalid" in
     let internal_typ =
       match internal.mod_desc with
       | Tmod_structure str -> List.hd str.str_items
       | _ -> failwith "invalid" in
     let internal_sig =
       match internal.mod_type with
       | Mty_signature [typ] -> typ
       | _ -> failwith "invalid" in
     let external_sig =
       match external_sig with
       | Mty_signature [typ] -> typ
       | _ -> failwith "invalid" in

     let _internal_typ_id, internal_typ_decl =
       match internal_typ.str_desc with
       | Tstr_type (_, [decl]) -> (decl.typ_id, decl.typ_type)
       | _ -> failwith "invalid" in
     let _internal_sig_id, internal_sig_decl =
       match internal_sig with
       | Sig_type (id, decl, _rec, _visibility) -> (id, decl)
       | _ -> failwith "invalid" in
     let _external_sig_id, external_sig_decl =
       match external_sig with
       | Sig_type (id, decl, _rec, _visibility) -> (id, decl)
       | _ -> failwith "invalid" in

     Format.printf
       "internal_typ_id: %a\ninternal_sig_id: %a\nexternal_sig_id: %a\n%!"
       Types.Uid.print internal_typ_decl.type_uid Types.Uid.print
       internal_sig_decl.type_uid Types.Uid.print external_sig_decl.type_uid;
     () *)
