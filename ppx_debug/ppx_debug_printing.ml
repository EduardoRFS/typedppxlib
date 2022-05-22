open Typedppxlib_ocaml_typing

(* TODO: this just force to patch stuff like read_cmi *)
include Typedppxlib

let () = Printexc.record_backtrace false

(* TODO: better API for lazy printing *)
(* TODO: probably IR for types also *)
let fprintf = Format.fprintf

(* TODO: OCaml can also be instrumented to have the types be known
         at runtime for handling polymorphism*)
(* TODO: this could actually be a generated piece of bytecode / native OCaml
         as there is no allocation it is mostly safe *)
type name = string

(* module Cycle_tbl : sig
     type t
     val make : unit -> t
     val mark : t -> 'a -> unit
     val find : t -> 'a -> int option
   end = struct
     type id = int
     type content = {
       key : Obj.t;
       id : id;
     }
     type t = {
       mutable next_id : id;
       table : (Obj.t, content) Hashtbl.t;
     }

     let make () =
       let next_id = 0 in
       let table = Hashtbl.create 16 in
       { next_id; table }

     let find t obj =
       match Hashtbl.find_opt t.table obj with
       | Some content ->
         if content.key == obj then
           Some content
         else
           List.find_opt
             (fun content -> content.key == obj)
             (Hashtbl.find_all t.table obj)
       | None -> None
     let mem t obj =
       match find t obj with
       | Some _content -> true
       | None -> false

     let mark t obj =
       let obj = Obj.repr obj in
       if Obj.is_int obj || mem t obj then
         ()
       else
         let id = t.next_id in
         t.next_id <- id + 1;
         let content = { key = obj; id } in
         Hashtbl.add t.table obj content
     let find t obj =
       let obj = Obj.repr obj in
       match find t obj with
       | Some content -> Some content.id
       | None -> None
   end *)

module Printing_ir = struct
  (* IR to simplify printing *)
  (* TOOD: how much of it should be built on compile time? *)
  type t = {
    typ : name;
    mutable desc : desc;
    mutable print : (Format.formatter -> Obj.t -> unit) option;
  }
  and desc =
    | Temp
    | Int
    | Char
    | String
    | Bytes
    | Float
    | Bool
    | Unit
    | Exn
    | Array         of t
    | List          of t
    | Option        of t
    | Nativeint
    | Int32
    | Int64
    | Lazy_t        of t
    | Var           of { name : string option }
    | Arrow
    (* may contain a single value *)
    | Tuple         of { fields : t list }
    | Abstract
    (* TODO: object, polyvars and modules *)
    (* TODO: record and variant  representation*)
    | Unimplemented
    | Record        of {
        unboxed : bool;
        fields : (name * t) list;
      }
    | Variant       of {
        unboxed : bool;
        constants : name array;
        blocks : (name * desc) array;
      }

  (* | Delayed of { mutable typ: Types.type_expr } *)
  let make typ desc = { typ; desc; print = None }
  let unsafe_of_string string : t = Marshal.from_string string 0
  let to_string t = Marshal.to_string t []
end
module Int_map = Map.Make (Int)
module Translate_typ = struct
  open Types

  module Path_map = Map.Make (Path)
  module Ids_map = Map.Make (struct
    type t = int list
    let compare = compare
  end)
  module Translate_context = struct
    type t = {
      env : Env.t;
      transparent_env : Ppx_debug_transparent_env.t;
      mutable types : Printing_ir.t Int_map.t;
      mutable constructors : Printing_ir.t Ids_map.t Path_map.t;
    }

    let make ~env ~transparent_env =
      {
        env;
        transparent_env;
        types = Int_map.empty;
        constructors = Path_map.empty;
      }

    let add_type t typ ir = t.types <- Int_map.add typ.id ir t.types
    let find_type t typ = Int_map.find_opt typ.id t.types

    let ids_of_args typ =
      List.rev_map (fun typ -> (Transient_expr.repr typ).id) typ
    let add_constr path args ir t =
      let rev_args_map =
        match Path_map.find_opt path t.constructors with
        | Some rev_args_map -> rev_args_map
        | None -> Ids_map.empty in

      let rev_args = ids_of_args args in
      let rev_args_map = Ids_map.add rev_args ir rev_args_map in
      t.constructors <- Path_map.add path rev_args_map t.constructors

    let find_constr t path args =
      match Path_map.find_opt path t.constructors with
      | Some rev_args_map -> (
        let rev_args = ids_of_args args in

        match Ids_map.find_opt rev_args rev_args_map with
        | Some printing_ir -> Some printing_ir
        | None -> None)
      | None -> None
  end
  let ir typ =
    let typ =
      Format.asprintf "%a" Printtyp.type_expr (Transient_expr.type_expr typ)
    in
    Printing_ir.make typ

  let apply_typ_args env ~params ~args typ =
    let open Ctype in
    (* TODO: should I use apply here? *)
    (* TODO: this is needed to ensure that instance generates a new instance *)
    let params, typ = instance_parameterized_type ~keep_names:true params typ in

    (* TODO: this may fail *)
    let param_arg_pairs = List.combine params args in
    List.iter (fun (param, arg) -> unify env param arg) param_arg_pairs;
    Transient_expr.repr typ

  let apply_typ_fields env ~params ~args fields =
    let open Ctype in
    begin_def ();
    let typ = newty (Ttuple fields) in
    end_def ();
    generalize typ;
    apply_typ_args env ~params ~args typ

  let apply_typ_labels env ~params ~args labels =
    let fields = List.map (fun ld -> ld.ld_type) labels in
    let body = apply_typ_fields env ~params ~args fields in
    match body.desc with
    | Ttuple types -> List.combine labels types
    | _ -> assert false

  let rec translate_typ ctx typ =
    let typ = Transient_expr.repr typ in

    match Translate_context.find_type ctx typ with
    | Some ir -> ir
    | None ->
    (* TODO: this is clearly bad code *)
    match typ.desc with
    | Tconstr (path, args, _abbrev) -> (
      match Translate_context.find_constr ctx path args with
      | Some ir -> ir
      | None ->
        let ir = ir typ Temp in

        Translate_context.add_type ctx typ ir;
        Translate_context.add_constr path args ir ctx;

        let desc = translate_typ_desc ctx typ in
        ir.desc <- desc;

        ir)
    | _ ->
      let ir = ir typ Unit in

      Translate_context.add_type ctx typ ir;

      let desc = translate_typ_desc ctx typ in
      ir.desc <- desc;

      ir

  and translate_typ_desc ctx typ =
    match typ.desc with
    (* TODO: any way to print vars? *)
    | Tvar name -> Var { name }
    | Tarrow _ -> Arrow
    | Ttuple fields ->
      let fields = List.map (fun typ -> translate_typ ctx typ) fields in
      Tuple { fields }
    (* TODO: what to use the abbrev *)
    | Tconstr (path, args, _abbrev) -> translate_constr ctx path args
    | Tobject _
    | Tfield _
    | Tnil ->
      Unimplemented
    | Tlink _ -> failwith "unreachable"
    (* TODO: when is Tsubst used? *)
    | Tsubst _
    | Tvariant _
    | Tunivar _
    | Tpoly _
    | Tpackage _ ->
      Unimplemented

  and translate_constr ctx path args =
    let open Predef in
    match (path, args) with
    | _, [] when path = path_int -> Int
    | _, [] when path = path_char -> Char
    | _, [] when path = path_string -> String
    | _, [] when path = path_bytes -> Bytes
    | _, [] when path = path_float -> Float
    | _, [] when path = path_bool -> Bool
    | _, [] when path = path_unit -> Unit
    | _, [] when path = path_exn -> Exn
    | _, [arg] when path = path_array -> Array (translate_typ ctx arg)
    | _, [arg] when path = path_list -> List (translate_typ ctx arg)
    | _, [arg] when path = path_option -> Option (translate_typ ctx arg)
    | _, [] when path = path_nativeint -> Nativeint
    | _, [] when path = path_int32 -> Int32
    | _, [] when path = path_int64 -> Int64
    | _, [arg] when path = path_lazy_t -> Lazy_t (translate_typ ctx arg)
    | _ -> (
      let decl =
        match
          Ppx_debug_transparent_env.lookup_type path ctx.transparent_env
        with
        | Some type_ -> type_
        | None -> Env.find_type path ctx.env in
      match decl.type_kind with
      | Type_abstract -> translate_abstract ctx decl args
      | Type_record (fields, representation) ->
        translate_record ctx decl fields representation args
      | Type_variant (constructors, representation) ->
        translate_variant ctx decl constructors representation args
      | Type_open -> Unimplemented)

  and translate_abstract ctx decl args =
    match decl.type_manifest with
    | Some body ->
      let params = decl.type_params in
      let body = apply_typ_args ctx.env ~params ~args body in

      translate_typ_desc ctx body
    | None -> Abstract

  and translate_record ctx decl fields representation args =
    (* TODO: this function looks very bad *)
    let unboxed =
      match representation with
      | Record_unboxed _inlined -> true
      | Record_regular
      | Record_float
      | Record_inlined _
      | Record_extension _ ->
        false in

    let types = apply_typ_labels ctx.env ~params:decl.type_params ~args fields in
    let fields =
      List.map
        (fun (ld, typ) ->
          let name = Ident.name ld.ld_id in
          (name, translate_typ ctx typ))
        types in
    Record { unboxed; fields }

  and translate_variant ctx decl constructors representation args =
    let translate_typ typ = translate_typ ctx typ in
    let unboxed =
      match representation with
      | Variant_unboxed -> true
      | Variant_regular -> false in
    let constants, blocks =
      let rec loop constants blocks constructors =
        match constructors with
        | [] -> (List.rev constants, List.rev blocks)
        | ({ cd_args = Cstr_tuple []; _ } as constructor) :: constructors ->
          loop (constructor :: constants) blocks constructors
        | constructor :: constructors ->
          loop constants (constructor :: blocks) constructors in
      let consts, blocks = loop [] [] constructors in
      (Array.of_list consts, Array.of_list blocks) in
    let params = decl.type_params in
    let constants = Array.map (fun cd -> Ident.name cd.cd_id) constants in
    let blocks =
      Array.map
        (fun cd ->
          let name = Ident.name cd.cd_id in
          let body =
            match cd.cd_args with
            | Cstr_tuple fields -> (
              let desc = apply_typ_fields ctx.env ~params ~args fields in
              let desc = translate_typ_desc ctx desc in
              match (unboxed, desc) with
              | true, Tuple { fields = [content] } -> content.desc
              | true, _ -> failwith "invalid type, unboxed but tuple?"
              | false, _ -> desc)
            | Cstr_record labels ->
              let fields = apply_typ_labels ctx.env ~params ~args labels in
              let fields =
                List.map
                  (fun (ld, typ) -> (Ident.name ld.ld_id, translate_typ typ))
                  fields in
              (match (unboxed, fields) with
              | true, [_]
              | false, _ ->
                ()
              | true, _ -> failwith "invalid type, unboxed but many fields");
              Record { unboxed; fields } in
          (name, body))
        blocks in
    Variant { unboxed; constants; blocks }

  let translate_typ env transparent_env typ =
    let ctx = Translate_context.make ~env ~transparent_env in
    translate_typ ctx typ
end

module Printer = struct
  open Printing_ir

  (* TODO: also checks the format of the data before printing to avoid segfault
           on data using Obj.magic *)
  let primitive s fmt value =
    let value : 'a = Obj.magic value in
    fprintf fmt s value
  let primitive_str to_string fmt value =
    let value : 'a = Obj.magic value in
    fprintf fmt "%s" (to_string value)
  let primitive_pp s pp fmt value =
    let value : 'a = Obj.magic value in
    fprintf fmt s pp value
  let opaque s typ fmt _value =
    let print fmt () = fprintf fmt s typ in
    fprintf fmt "\"%a\"" print ()
  let unimplemented typ = opaque "<unimplemented:%s>" typ

  (* TODO: does abbrev matter here? *)
  let print_hex fmt bytes =
    Bytes.iter (fun char -> Format.fprintf fmt "%X" (Char.code char)) bytes
  let print_array f fmt array =
    let printf s = Format.fprintf fmt s in
    printf "[|";
    let last = Array.length array - 1 in
    Array.iteri
      (fun i el ->
        printf "%a" f el;
        if i <> last then printf "; ")
      array;
    printf "|]"
  let print_list f fmt l =
    let printf s = Format.fprintf fmt s in
    printf "[";
    let rec loop l =
      match l with
      | [] -> printf "]"
      | [el] -> printf "%a]" f el
      | el :: tl ->
        printf "%a; " f el;
        loop tl in
    loop l
  let print_option f fmt value =
    let printf s = Format.fprintf fmt s in
    match value with
    | Some value -> printf "(Some %a)" f value
    | None -> printf "None"
  let print_lazy typ arg fmt value =
    let value : 'a Lazy.t = Obj.magic value in
    if Lazy.is_val value then
      fprintf fmt "(lazy %a)" arg (Lazy.force_val value)
    else
      opaque "<lazy:%s>" typ fmt value

  let int = primitive "%d"
  let char = primitive "%C"
  let string = primitive "%S"
  let bytes = primitive_pp "0x%a" print_hex
  let float = primitive "%F"
  let bool = primitive "%b"
  let unit = primitive_str Unit.to_string
  let exn = primitive_str Printexc.to_string
  let array arg = primitive_pp "%a" (print_array arg)
  let list arg = primitive_pp "%a" (print_list arg)
  let option arg = primitive_pp "%a" (print_option arg)
  let nativeint = primitive_str Nativeint.to_string
  let int32 = primitive_str Int32.to_string
  let int64 = primitive_str Int64.to_string
  let lazy_t typ arg = primitive_pp "%a" (print_lazy typ arg)
  let arrow typ = opaque "<fun:%s>" typ
  let abstract typ = opaque "<abstract:%s>" typ
  let tuple fields fmt value =
    let value : 'a array = Obj.magic value in

    fprintf fmt "(";
    let rec loop i l =
      match l with
      | [] -> ()
      | print :: tl ->
        let field = Array.get value i in
        print fmt field;
        (match tl with
        | [] -> fprintf fmt ")"
        | _ -> fprintf fmt ", ");
        loop (i + 1) tl in
    loop 0 fields

  let record ~unboxed fields fmt value =
    let value : 'a array = Obj.magic value in

    fprintf fmt "{ ";
    let rec loop i l =
      match l with
      | [] -> ()
      | (name, print) :: tl ->
        let field =
          if unboxed then
            Obj.magic value
          else
            Array.get value i in
        fprintf fmt "%s = %a" name print field;
        (match tl with
        | [] -> fprintf fmt " }"
        | _ -> fprintf fmt "; ");
        loop (i + 1) tl in
    loop 0 fields

  let variant ~constants ~blocks fmt value =
    let value = Obj.repr value in
    if Obj.is_int value then
      let tag : int = Obj.obj value in
      let name = Array.get constants tag in
      fprintf fmt "%s" name
    else
      let tag = Obj.tag value in
      let name, print = Array.get blocks tag in
      fprintf fmt "(%s %a)" name print value

  (* TODO: cycle detection both on types and on values *)
  let rec print_value printing_ir =
    let { typ; desc; print } = printing_ir in
    match print with
    | Some print -> print
    | None ->
      (* setup *)
      let print_ref = ref (fun _ -> assert false) in
      printing_ir.print <- Some (fun fmt obj -> !print_ref fmt obj);

      (* construct *)
      let print = print_value_desc typ desc in
      print_ref := print;
      (* path compression *)
      printing_ir.print <- Some print;

      print

  and print_value_desc typ desc =
    match desc with
    | Temp -> fun fmt _ -> fprintf fmt "(temp found this is likely a bug)"
    | Int -> int
    | Char -> char
    | String -> string
    | Bytes -> bytes
    | Float -> float
    | Bool -> bool
    | Unit -> unit
    | Exn -> exn
    | Array arg -> array (print_value arg)
    | List arg -> list (print_value arg)
    | Option arg -> option (print_value arg)
    | Nativeint -> nativeint
    | Int32 -> int32
    | Int64 -> int64
    (* TODO: option to force the lazy if the user wants *)
    | Lazy_t arg -> lazy_t typ (print_value arg)
    | Var _ -> unimplemented typ
    | Arrow -> arrow typ
    | Tuple { fields } ->
      let fields = List.map print_value fields in
      tuple fields
    | Abstract -> abstract typ
    | Unimplemented -> unimplemented typ
    | Record { unboxed; fields } ->
      let fields = List.map (fun (name, ir) -> (name, print_value ir)) fields in
      record ~unboxed fields
    | Variant { unboxed; constants; blocks } ->
      (match (unboxed, constants, blocks) with
      | true, _, [|_|]
      | false, _, _ ->
        ()
      | true, _, _ -> failwith "bug");
      let blocks =
        Array.map
          (fun (name, ir) ->
            let print = print_value_desc typ ir in
            (name, print))
          blocks in
      variant ~constants ~blocks
end

type runtime_data
type partial_ir
let truly_unsafe_pp fmt ~runtime_data:_ ~(partial_ir : partial_ir) =
  (* TODO: receive id separated from runtime_data to avoid parsing
           also use the id to hold the print_typ function *)
  let partial_ir =
    let partial_ir : string = Obj.magic partial_ir in
    Printing_ir.unsafe_of_string partial_ir in
  let print = Printer.print_value partial_ir in
  fun value ->
    let value = Obj.repr value in
    Format.fprintf fmt "%a\n%!" print value

(* TODO: dedup and cyclical data using let *)
