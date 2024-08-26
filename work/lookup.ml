let failwithf = Myast.failwithf

type config = {
  mutable instructions_file : string;
  mutable instr_to_process : string;
  mutable riscv_td_file : string;
}

let config =
  {
    instructions_file = "./json/allkeys.txt";
    instr_to_process = "";
    riscv_td_file = "json/RISCV.instructions.json";
  }

let () =
  Arg.parse
    [
      ( "-i",
        Arg.String (fun s -> config.instr_to_process <- s),
        " Name of instruction to lookup" );
    ]
    (fun s -> config.instructions_file <- s)
    "help TODO"

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

let extract_operands_info key =
  let read_td_json filename =
    let j = In_channel.with_open_text filename Yojson.Safe.from_channel in
    match j with `Assoc xs -> xs | _ -> exit 1
  in
  let from_assoc = function `Assoc xs -> xs | _ -> assert false in
  let from_list = function `List xs -> xs | _ -> assert false in
  let j : (string * Yojson.Safe.t) list =
    match List.assoc key (read_td_json config.riscv_td_file) with
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
  print_endline "";
  Format.printf "@[In operands: %s@]\n%!" (String.concat " " in_operands);
  Format.printf "@[Out operands: %s@]\n%!" (String.concat " " out_operands);
  (in_operands, out_operands)

let process_single iname =
  print_endline __FUNCTION__;
  let mangled_iname =
    match iname with
    | "ADD_UW" -> "ADDUW"
    | _ -> (
        match Analyzer.smart_rewrite iname with Some x -> x | None -> iname)
  in
  let on_found iname =
    stats.from6159 <- stats.from6159 + 1;
    let _ = extract_operands_info iname in
    ()
  in
  let () =
    match iname with
    | s when is_omitted_explicitly s -> ()
    | _ ->
        if
          List.mem mangled_iname From6159.from6159
          || List.mem ("RISCV_" ^ mangled_iname) From6159.from6159
        then on_found iname
        else if
          List.mem ("RISCV_" ^ mangled_iname) From6159.from6159_hacky
          || List.mem mangled_iname From6159.from6159_hacky
        then on_found iname
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
