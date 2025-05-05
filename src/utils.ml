let chop_suffix ~suffix s =
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

let rm_duplicates list =
  let rec helper acc = function
    | [] -> acc
    | x :: xs -> if List.mem x acc then helper acc xs else helper (x :: acc) xs
  in
  helper [] list

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
  let { mnemonic; operands; ins; outs; mayLoad; mayStore; ins_csr; outs_csr } =
    instr
  in
  printf
    "@[Hashtbl.add ans \"%s\" { mnemonic=\"%s\"; operands=[%a]; ins=[%a]; \
     outs=[%a]; mayLoad=%B; mayStore=%B; ins_csr=[%a]; outs_csr=[%a] };@]@,"
    mnemonic mnemonic lst_str operands lst_str ins lst_str outs mayLoad mayStore
    lst_str ins_csr lst_str outs_csr

let profile_start () = Unix.gettimeofday ()

let profile_end msg start_time =
  let end_time = Unix.gettimeofday () in
  if debug then printfn "Exec time of %s: %f" msg (end_time -. start_time)
