open Typedppxlib_ocaml_typing

type runtime_data

(* TODO: better API for lazy printing *)
(* TODO: probably IR for types also *)
let fprintf = Format.fprintf

(* TODO: OCaml can also be instrumented to have the types be known
         at me for handling polymorphism*)
(* TODO: this could actually be a generated piece of bytecode / native OCaml
         as there is no allocation it is mostly safe *)
type name = string

module Printing_ir = struct
  (* IR to simplify printing *)
  (* TOOD: how much of it should be built on compile time? *)
  type t = {
    id : int;
    typ : name;
    desc : desc;
  }
  and desc =
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
        blocks : (name * t) array;
      }

  let make =
    let acc = ref 0 in
    fun typ desc ->
      let id = !acc in
      incr acc;
      { id; typ; desc }
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
    let { id = _; typ; desc } = printing_ir in
    match desc with
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
      let blocks = Array.map (fun (name, ir) -> (name, print_value ir)) blocks in
      variant ~constants ~blocks
end

module Translate_typ = struct
  open Types

  let ir typ =
    let typ = Format.asprintf "%a\n%!" Printtyp.type_expr typ in
    Printing_ir.make typ

  let apply_typ_fields env ~params ~args fields =
    let open Ctype in
    begin_def ();
    let typ = newty (Ttuple fields) in
    end_def ();
    generalize typ;

    (* TODO: should I use apply here? *)
    (* TODO: this is needed to ensure that instance generates a new instance *)
    let params, typ = instance_parameterized_type ~keep_names:true params typ in

    (* TODO: this may fail *)
    let param_arg_pairs = List.combine params args in
    List.iter (fun (param, arg) -> unify env param arg) param_arg_pairs;
    typ

  let apply_typ_labels env ~params ~args labels =
    let fields = List.map (fun ld -> ld.ld_type) labels in
    let body = apply_typ_fields env ~params ~args fields in
    match (Ctype.repr body).desc with
    | Ttuple types -> List.combine labels types
    | _ -> assert false

  let rec translate_typ env typ =
    let translate_typ typ = translate_typ env typ in

    match typ.desc with
    (* TODO: any way to print vars? *)
    | Tvar name -> ir typ (Var { name })
    | Tarrow _ -> ir typ Arrow
    | Ttuple fields ->
      let fields = List.map translate_typ fields in
      ir typ (Tuple { fields })
    (* TODO: what to use the abbrev *)
    | Tconstr (path, args, _abbrev) -> translate_constr env typ path args
    | Tobject _
    | Tfield _
    | Tnil ->
      ir typ Unimplemented
    | Tlink typ -> translate_typ typ
    (* TODO: when is Tsubst used? *)
    | Tsubst _
    | Tvariant _
    | Tunivar _
    | Tpoly _
    | Tpackage _ ->
      ir typ Unimplemented

  and translate_constr env typ path args =
    let open Predef in
    let translate_typ typ = translate_typ env typ in
    let ir desc = ir typ desc in
    match (path, args) with
    | _, [] when path = path_int -> ir Int
    | _, [] when path = path_char -> ir Char
    | _, [] when path = path_string -> ir String
    | _, [] when path = path_bytes -> ir Bytes
    | _, [] when path = path_float -> ir Float
    | _, [] when path = path_bool -> ir Bool
    | _, [] when path = path_unit -> ir Unit
    | _, [] when path = path_exn -> ir Exn
    | _, [arg] when path = path_array -> ir (Array (translate_typ arg))
    | _, [arg] when path = path_list -> ir (List (translate_typ arg))
    | _, [arg] when path = path_option -> ir (Option (translate_typ arg))
    | _, [] when path = path_nativeint -> ir Nativeint
    | _, [] when path = path_int32 -> ir Int32
    | _, [] when path = path_int64 -> ir Int64
    | _, [arg] when path = path_lazy_t -> ir (Lazy_t (translate_typ arg))
    | _ -> (
      let decl = Env.find_type path env in
      match decl.type_kind with
      | Type_abstract -> translate_abstract env typ decl args
      | Type_record (fields, representation) ->
        translate_record env typ decl fields representation args
      | Type_variant (constructors, representation) ->
        translate_variant env typ decl constructors representation args
      | Type_open -> ir Unimplemented)

  and translate_abstract env typ decl args =
    let open Ctype in
    match decl.type_manifest with
    | Some body ->
      (* TODO: should I use apply here? *)
      let params, body =
        instance_parameterized_type ~keep_names:true decl.type_params body in

      (* TODO: this may fail *)
      let param_arg_pairs = List.combine params args in
      List.iter (fun (param, arg) -> unify env param arg) param_arg_pairs;

      translate_typ env body
    | None -> ir typ Abstract

  and translate_record env typ decl fields representation args =
    (* TODO: this function looks very bad *)
    let unboxed =
      match representation with
      | Record_unboxed _inlined -> true
      | Record_regular
      | Record_float
      | Record_inlined _
      | Record_extension _ ->
        false in

    let types = apply_typ_labels env ~params:decl.type_params ~args fields in
    let fields =
      List.map
        (fun (ld, typ) ->
          let name = Ident.name ld.ld_id in
          (name, translate_typ env typ))
        types in
    ir typ (Record { unboxed; fields })

  and translate_variant env typ decl constructors representation args =
    let translate_typ typ = translate_typ env typ in
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
              let body = apply_typ_fields env ~params ~args fields in
              let body = translate_typ body in
              match (unboxed, body.desc) with
              | true, Tuple { fields = [content] } -> content
              | true, _ -> failwith "invalid type, unboxed but tuple?"
              | false, _ -> body)
            | Cstr_record labels ->
              let fields = apply_typ_labels env ~params ~args labels in
              let fields =
                List.map
                  (fun (ld, typ) -> (Ident.name ld.ld_id, translate_typ typ))
                  fields in
              (match (unboxed, fields) with
              | true, [_]
              | false, _ ->
                ()
              | true, _ -> failwith "invalid type, unboxed but many fields");
              ir typ (Record { unboxed; fields }) in
          (name, body))
        blocks in
    ir typ (Variant { unboxed; constants; blocks })
end

let truly_unsafe_print ~(runtime_data : runtime_data) =
  (* TODO: receive id separated from runtime_data to avoid parsing
           also use the id to hold the print_typ function *)
  let env, type_expr =
    let open Ppx_debug_runtime_data in
    let runtime_data : string = Obj.magic runtime_data in
    let runtime_data = unsafe_of_string runtime_data in
    (env runtime_data, type_expr runtime_data) in
  let ir = Translate_typ.translate_typ env type_expr in
  let print = Printer.print_value ir in
  fun value ->
    let value = Obj.repr value in
    Format.printf "%a\n%!" print value
