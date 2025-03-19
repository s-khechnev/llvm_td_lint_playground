open Core

type config = {
  mutable riscv_td_json : string;
  mutable ocaml_ident : string;
  mutable ocaml_code : string;
}

let config = { riscv_td_json = ""; ocaml_ident = ""; ocaml_code = "" }

let read_td_json filename =
  let j = In_channel.with_open_text filename Yojson.Safe.from_channel in
  match j with `Assoc xs -> xs | _ -> assert false

let extract_operands_info (j : (string * Yojson.Safe.t) list) =
  let operands =
    match List.assoc "AsmString" j with
    | `String asm_str -> (
        match String.split_on_char '\t' asm_str with
        | _ :: [ operands ] ->
            operands |> String.split_on_char ','
            |> List.concat_map (String.split_on_char '$')
            |> List.filter_map (fun s ->
                   let s = String.trim s in
                   if String.equal s "" then None
                   else
                     let regex = Str.regexp "{\\(.*?\\)}" in
                     if Str.string_match regex s 0 then
                       Some (Str.matched_group 1 s)
                     else Some s)
        | _ -> [])
    | _ -> assert false
  in
  let from_assoc = function `Assoc xs -> xs | _ -> assert false in
  let from_list = function `List xs -> xs | _ -> assert false in
  let exract_operands j =
    j |> from_assoc |> List.assoc "args" |> from_list
    |> List.map (function
         | `List [ `Assoc [ _; _; _ ]; `String s ] ->
             Utils.chop_suffix s ~suffix:"_wb"
         | other ->
             Format.printf "Unsupported case: %a\n"
               (Yojson.Safe.pretty_print ~std:false)
               other;
             assert false)
  in
  let in_operands = exract_operands (List.assoc "InOperandList" j) in
  let out_operands = exract_operands (List.assoc "OutOperandList" j) in
  (operands, out_operands, in_operands)

let dump_llvm () =
  let llvm_json = read_td_json config.riscv_td_json in
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[open Core.Instruction@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 1000 in@]@ ";

      let lst_str ppf out =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          (fun ppf -> Format.fprintf ppf "%S")
          ppf out
      in
      List.iter
        (fun (mnemonic, j) ->
          let j = match j with `Assoc xs -> xs | _ -> assert false in
          let operands, outs, ins = extract_operands_info j in
          printf
            "@[Hashtbl.add ans \"%s\" { mnemonic=\"%s\"; operands=[%a]; \
             ins=[%a]; outs=[%a] };@]@,"
            mnemonic mnemonic lst_str operands lst_str ins lst_str outs)
        llvm_json;
      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let lookup_exn = Hashtbl.find %s @]@," config.ocaml_ident;
      printf "@[let mem = Hashtbl.mem %s @]@," config.ocaml_ident)

let () =
  Arg.parse
    [
      ("-dump-llvm", Arg.String (fun s -> config.riscv_td_json <- s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun _ -> failwith "Bad argument: %S")
    "";
  dump_llvm ()
