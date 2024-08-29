let failwithf = Myast.failwithf

type config = {
  mutable instructions_file : string;
  mutable instr_to_process : string;
  mutable riscv_td_file : string;
  mutable sail_json : string;
  mutable verbose : bool;
}

let config =
  {
    instructions_file = "./json/allkeys.txt";
    instr_to_process = "";
    riscv_td_file = "json/RISCV.instructions.json";
    sail_json = "work/tmp/06159.json";
    verbose = false;
  }

let () =
  Arg.parse
    [
      ( "-i",
        Arg.String (fun s -> config.instr_to_process <- s),
        " Name of instruction to lookup" );
      ("-sail-json", Arg.String (fun s -> config.sail_json <- s), " ");
      ("-v", Arg.Unit (fun () -> config.verbose <- true), " ");
    ]
    (fun s -> config.instructions_file <- s)
    "help TODO"

let log fmt =
  if config.verbose then Format.kasprintf print_endline fmt
  else Format.ikfprintf (fun _ -> ()) Format.std_formatter fmt

type stats = {
  mutable from6159 : int;
  mutable total : int;
  mutable first_unknown : string list;
}

let stats = { from6159 = 0; total = 0; first_unknown = [] }

let save_unknown orig s =
  let s = if orig <> s then Printf.sprintf "%s(%s)" s orig else s in
  match stats.first_unknown with
  | [] -> stats.first_unknown <- [ s ]
  | [ a ] -> stats.first_unknown <- [ a; s ]
  | [ a; b ] -> stats.first_unknown <- [ a; b; s ]
  | [ a; b; c ] -> stats.first_unknown <- [ a; b; c; s ]
  | _ -> ()

let report () =
  let open Format in
  printf "Total instructions: %d\n" stats.total;
  printf "from6159: %d\n" stats.from6159;
  printf "First unknown: %s\n" (String.concat " " stats.first_unknown)

let omitted_explicitly = [ ""; "ANNOTATION_LABEL" ]

let is_omitted_explicitly s =
  let bad_prefixes =
    [
      (* AMOADD_D* and AMOADD_B* not found in sail *)
      "AMOADD_";
      "AMOAND_";
      "AMOCAS_";
      "DIVU";
      "DRET";
      (* Can't find CBO in SAIL *)
      "CBO_";
      (* Can't find Zcmt in SAIL *)
      "CM_";
      (* CSR is not defined in  riscv_csr_ext.sail *)
      "CSRR";
      (* I don't even know what it is  *)
      "CV_";
      (* Zimop https://github.com/riscv/riscv-isa-manual/blob/main/src/zimop.adoc *)
      "C_MOP";
      (* Compresed instructions will be difficult to support
         because they call recursively to other instructions.
         riscv_insts_zca.sail  253 *)
      "C_";
    ]
  in
  Option.is_some
  @@ List.find_opt (fun prefix -> String.starts_with ~prefix s) bad_prefixes
  || List.mem s omitted_explicitly

let read_td_json filename =
  let j = In_channel.with_open_text filename Yojson.Safe.from_channel in
  match j with `Assoc xs -> xs | _ -> exit 1

let llvm_JSON = lazy (read_td_json config.riscv_td_file)

let extract_operands_info key =
  let from_assoc = function `Assoc xs -> xs | _ -> assert false in
  let from_list = function `List xs -> xs | _ -> assert false in
  let j : (string * Yojson.Safe.t) list =
    match List.assoc key (Lazy.force llvm_JSON) with
    | `Assoc xs -> xs
    | exception Not_found ->
        Printf.eprintf "Can't get key %S\n" key;
        exit 1
    | _ -> assert false
  in
  let exract_operands j =
    j |> from_assoc |> List.assoc "args" |> from_list
    |> List.map (function
         | `List [ `Assoc [ _; _; _ ]; `String s ] -> s
         | other ->
             Myast.failwithf "Unsupported case: %a\n"
               (Yojson.Safe.pretty_print ~std:false)
               other)
  in
  let in_operands = exract_operands (List.assoc "InOperandList" j) in
  let out_operands = exract_operands (List.assoc "OutOperandList" j) in
  if config.verbose then (
    print_endline "";
    Format.printf "@[In operands: %s@]\n%!" (String.concat " " in_operands);
    Format.printf "@[Out operands: %s@]\n%!" (String.concat " " out_operands));
  (in_operands, out_operands)

let chop_suffix ~suffix s =
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

let sail_AST =
  lazy
    (let j =
       In_channel.with_open_text config.sail_json Yojson.Safe.from_channel
     in
     match Myast.def_of_yojson Myast.tannot_of_yojson j with
     | Result.Error err ->
         Format.eprintf "Error: %s\n%!" err;
         exit 1
     | Ok ast -> ast)

let get_sail_clause cname =
  let open Myast in
  let exception
    Found of
      Libsail.Type_check.tannot Libsail.Ast.pat list
      * Libsail.Type_check.tannot Myast.exp
  in
  try
    match Lazy.force sail_AST with
    | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, body), _)), _) ->
        List.iter
          (function
            | Myast.FCL_aux (FCL_funcl (Id_aux (Id "execute", _), fdecl), _)
              -> (
                match fdecl with
                | Pat_aux
                    ( Pat_exp
                        (P_aux (P_app (Id_aux (Id name, _), pargs), _), exp),
                      _ )
                  when name = cname ->
                    raise (Found (pargs, exp))
                | _ -> ())
            | _ -> ())
          body;
        failwithf "%s %d" __FILE__ __LINE__
    | _ -> failwithf "%s %d" __FILE__ __LINE__
  with Found (pats, expr) -> (pats, expr)

let process_single iname =
  let mangled_iname =
    match iname with
    | "ADD_UW" -> "ADDUW"
    | "FADD_H" | "FADD_D" -> iname
    | s when String.ends_with ~suffix:"_INX" s -> chop_suffix ~suffix:"_INX" s
    | s when String.ends_with ~suffix:"_IN32X" s ->
        chop_suffix ~suffix:"_IN32X" s
    | _ -> (
        match Analyzer.smart_rewrite iname with Some x -> x | None -> iname)
  in
  let on_found iname ~mangled =
    stats.from6159 <- stats.from6159 + 1;
    let info = Hashtbl.find From6159.from6159 mangled in
    let _ = extract_operands_info iname in
    let top_cname =
      match info with
      | From6159.CI_default s -> s
      | From6159.CI_hacky (s, _) -> s
    in
    let _ = get_sail_clause top_cname in
    log "Got a SAIL clause";
    ()
  in
  let () =
    match iname with
    | s when is_omitted_explicitly s -> ()
    | _ ->
        if Hashtbl.mem From6159.from6159 mangled_iname then
          on_found iname ~mangled:mangled_iname
        else if Hashtbl.mem From6159.from6159 ("RISCV_" ^ mangled_iname) then
          on_found iname ~mangled:("RISCV_" ^ mangled_iname)
          (* else if
               Hashtbl.mem ("RISCV_" ^ mangled_iname) From6159.from6159_hacky
               || Hashtbl.mem mangled_iname From6159.from6159_hacky
             then on_found iname *)
        else save_unknown iname mangled_iname
  in
  ()

let () =
  if config.instr_to_process <> "" then (
    process_single config.instr_to_process;
    report ())
  else
    let all_iname =
      In_channel.with_open_text config.instructions_file In_channel.input_all
      |> String.split_on_char '\n'
    in
    stats.total <- List.length all_iname;
    ListLabels.iter all_iname ~f:process_single;
    report ()
