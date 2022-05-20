open Typedppxlib_ocaml_typing
open Typedppxlib_ocaml_utils

module Id : sig
  type t
  val next : unit -> t
  val equal : t -> t -> bool

  val unsafe_of_string : string -> t
  val to_string : t -> string
end = struct
  (* TODO: can this be a problem in multicore? *)
  type t = int
  let acc = ref 0
  let equal = Int.equal
  let next () =
    incr acc;
    !acc

  let unsafe_of_string string = int_of_string string
  let to_string t = string_of_int t
end

(* TODO: include paths only if needed for a specific type *)
type t = {
  (* TODO: is complete shell env needed? *)
  id : Id.t;
  cwd : string;
  paths : string list;
  env : Env.t;
}
let make ~env =
  (* TODO: should this be here? *)
  let id = Id.next () in
  let cwd = Sys.getcwd () in
  let paths = Load_path.get_paths () in
  { id; cwd; paths; env }
let id t = t.id
let cwd t = t.cwd
let paths t = t.paths
let env t = t.env

let unsafe_of_string string =
  let { id; cwd; paths; env } = Marshal.from_string string 0 in
  let env = Env.keep_only_summary env in
  let paths =
    List.map
      (fun file ->
        if Filename.is_relative file then Filename.concat cwd file else file)
      paths in

  (* TODO: should this be here??? *)
  (* TODO: instantiate the type to ensure the ids
     are correct in this environment? How does CMI loading does that?*)
  let () =
    Load_path.reset ();
    List.iter
      (fun path ->
        let dir = Load_path.Dir.create path in
        Load_path.append_dir dir)
      paths;
    Env.reset_cache () in
  let env = Envaux.env_of_only_summary env in

  { id; cwd; paths; env }
let to_string t =
  let { id; cwd; paths; env } = t in
  let env = Env.keep_only_summary env in
  let t = { id; cwd; paths; env } in
  (* TODO: if too big compress *)
  Marshal.to_string t []
