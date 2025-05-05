open Checker_core
open Utils

type config = {
  mutable riscv_td_json : string;
  mutable ocaml_ident : string;
  mutable ocaml_code : string;
  mutable out_json : string;
}

let config =
  { riscv_td_json = ""; ocaml_ident = ""; ocaml_code = ""; out_json = "" }

let from_assoc = function `Assoc xs -> xs | _ -> assert false
let from_list = function `List xs -> xs | _ -> assert false
let from_int = function `Int d -> d | _ -> assert false

let extract_info (j : (string * Yojson.Safe.t) list) =
  let mnemonic, operands =
    match List.assoc "AsmString" j with
    | `String asm_str -> (
        match String.split_on_char '\t' asm_str with
        | mnemonic :: [ operands ] ->
            ( mnemonic,
              operands |> String.split_on_char ','
              |> List.concat_map (String.split_on_char '$')
              |> List.filter_map (fun s ->
                     let s = String.trim s in
                     if String.equal s "" then None
                     else
                       let regex = Str.regexp "{\\(.*?\\)}" in
                       if Str.string_match regex s 0 then
                         Some (Str.matched_group 1 s)
                       else Some s) )
        | _ -> (asm_str, []))
    | _ -> assert false
  in
  let exract_operands j =
    j |> from_assoc |> List.assoc "args" |> from_list
    |> List.map (function
         | `List [ `Assoc [ _; _; _ ]; `String s ] ->
             chop_suffix s ~suffix:"_wb"
         | other ->
             Format.printf "Unsupported case: %a\n"
               (Yojson.Safe.pretty_print ~std:false)
               other;
             assert false)
  in
  let extract_implicit_gprs j =
    let map = function
      | "X0" -> "zreg"
      | "X1" -> "ra"
      | "X2" -> "sp"
      | s -> s
    in
    j |> from_list
    |> List.filter_map (function
         | `Assoc [ ("def", `String s); _; _ ] ->
             let is_gpr str =
               Str.string_match
                 (Str.regexp "^X\\([0-9]\\|[12][0-9]\\|3[01]\\)$")
                 str 0
             in
             if is_gpr s then Some (map s) else None
         | other ->
             Format.printf "Unsupported case: %a\n"
               (Yojson.Safe.pretty_print ~std:false)
               other;
             assert false)
  in
  let in_operands =
    let explicit = exract_operands (List.assoc "InOperandList" j) in
    let implicit = extract_implicit_gprs (List.assoc "Uses" j) in
    explicit @ implicit
  in
  let out_operands =
    let explicit = exract_operands (List.assoc "OutOperandList" j) in
    let implicit = extract_implicit_gprs (List.assoc "Defs" j) in
    explicit @ implicit
  in
  let arch =
    let open Checker_core.Instruction.Arch in
    match List.assoc "Predicates" j with
    | `List ps ->
        if List.is_empty ps then RV32_RV64
        else
          let f s =
            List.exists
              (function
                | `Assoc [ ("def", `String s'); _; _ ] when s = s' -> true
                | _ -> false)
              ps
          in
          if f "IsRV32" then RV32 else if f "IsRV64" then RV64 else RV32_RV64
    | _ -> assert false
  in
  let mayLoad, mayStore =
    let int_to_bool x = x <> 0 in
    let f s = List.assoc s j |> from_int |> int_to_bool in
    (f "mayLoad", f "mayStore")
  in
  (mnemonic, operands, out_operands, in_operands, arch, mayLoad, mayStore)

let is_good_data = function
  | `Assoc xs ->
      let is_instruction, is_pseudo =
        match List.assoc_opt "!superclasses" xs with
        | Some (`List classes) ->
            let mem x = List.mem (`String x) classes in
            (mem "Instruction", mem "Pseudo" || mem "StandardPseudoInstruction")
        | _ -> (false, false)
      in
      let is_anonymous =
        match List.assoc_opt "!anonymous" xs with
        | Some (`Bool b) -> b
        | _ -> false
      in
      let is_code_gen_only =
        match List.assoc_opt "isCodeGenOnly" xs with
        | Some (`Int v) -> v = 1
        | _ -> false
      in
      let is_null_or_empty_asm_string =
        match List.assoc_opt "AsmString" xs with
        | Some `Null -> true
        | Some (`String s) -> s = ""
        | _ -> false
      in
      let from_p_ext =
        match List.assoc_opt "!locs" xs with
        | Some (`List xs) ->
            List.exists
              (function
                | `String s ->
                    Str.string_match
                      (Str.regexp_string "RISCVInstrInfoP.td")
                      s 0
                | _ -> assert false)
              xs
        | _ -> false
      in
      is_instruction && (not is_pseudo) && (not is_anonymous)
      && (not is_code_gen_only)
      && (not is_null_or_empty_asm_string)
      && from_p_ext
  | _ -> false

let dump_llvm () =
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[open Checker_core.Instruction@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = InstrTable.create 1000 in@]@ ";

      let xs =
        from_assoc
          (In_channel.with_open_text config.riscv_td_json
             Yojson.Safe.from_channel)
      in
      let filtered =
        List.filter_map
          (fun (k, j) ->
            if is_good_data j then (
              let j =
                `Assoc
                  (List.filter
                     (fun (k, _) ->
                       not
                         (List.mem k
                            [ "SoftFail"; "TSFlags"; "SchedRW"; "Inst" ]))
                     (from_assoc j))
              in
              let xs = from_assoc j in
              let mnemonic, operands, outs, ins, arch, mayLoad, mayStore =
                extract_info xs
              in
              let ins_csr, outs_csr = ([], []) in
              Utils.printf_add_instr ppf
                {
                  mnemonic;
                  arch;
                  operands;
                  ins;
                  outs;
                  mayLoad;
                  mayStore;
                  ins_csr;
                  outs_csr;
                };
              Some (k, j))
            else None)
          xs
      in
      Out_channel.with_open_text config.out_json (fun ch_filtered ->
          (Yojson.Safe.pretty_to_channel ch_filtered) (`Assoc filtered));

      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();

      printf "@[let lookup_exn = InstrTable.find %s @]@," config.ocaml_ident;
      printf "@[let mem = InstrTable.mem %s @]@," config.ocaml_ident)

let () =
  Arg.parse
    [
      ("-dump-llvm", Arg.String (fun s -> config.riscv_td_json <- s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
      ("-o-filtered", Arg.String (fun s -> config.out_json <- s), "");
    ]
    (fun _ -> failwith "Bad argument: %S")
    "";
  dump_llvm ()
