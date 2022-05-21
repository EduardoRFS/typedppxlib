(*TODO: actually ensure the AST generated *)
(* TODO: [%e] or not? *)
let%define [%ID x] = [%e x]
let () = [%ID ()]

let%define [%ADD a, b] = [%e a] + [%e b]

module M = struct
  let%define [%ID x] = ()
  let%define [%ONE] = 1
end

open! M
let () = [%ID 1]
let () = assert ([%ONE] = 1)
let () = assert ([%ADD 1, 2] = 3)
let () = Format.printf "%d\n%!" [%ONE]
(* let%ppx [%ID x] = x
   let () = [%ID ()] *)
