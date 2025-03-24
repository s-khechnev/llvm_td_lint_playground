let chop_suffix ~suffix s =
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

let failwithf fmt = Format.kasprintf failwith fmt
let printfn ppf = Format.kasprintf print_endline ppf
let debug = false
let log fmt = if debug then Format.kasprintf print_endline fmt

let printf_add_instr ppf instr =
  let printf fmt = Format.fprintf ppf fmt in
  let lst_str ppf out =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
      (fun ppf -> Format.fprintf ppf "%S")
      ppf out
  in
  let open Instruction in
  let { mnemonic; operands; ins; outs; mayLoad; mayStore } = instr in
  printf
    "@[Hashtbl.add ans \"%s\" { mnemonic=\"%s\"; operands=[%a]; ins=[%a]; \
     outs=[%a]; mayLoad=%B; mayStore=%B };@]@,"
    mnemonic mnemonic lst_str operands lst_str ins lst_str outs mayLoad mayStore
