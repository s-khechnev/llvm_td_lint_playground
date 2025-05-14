open Checker_core
open Utils
open Instruction

let () =
  Llvm_info.llvm_info |> InstrTable.to_seq |> List.of_seq
  |> List.sort_uniq (fun ((a1, m1), _) ((a2, m2), _) ->
         match compare m1 m2 with 0 -> Arch.compare a1 a2 | n -> n)
  |> List.iter (fun ((arch, iname), llvm_instr) ->
         let {
           mnemonic = llvm_mnemonic;
           arch = llvm_arch;
           operands = llvm_opers;
           ins = llvm_ins;
           outs = llvm_outs;
         } : Instruction.t =
           llvm_instr
         in
         let llvm_mnemonic =
           if String.ends_with iname ~suffix:"aqrl" then
             let iname = Utils.chop_suffix ~suffix:"rl" llvm_mnemonic in
             iname ^ ".rl"
           else llvm_mnemonic
         in
         let process sail_instr =
           let {
             mnemonic = _;
             arch = sail_arch;
             operands = sail_opers;
             ins = sail_ins;
             outs = sail_outs;
           } : Instruction.t =
             sail_instr
           in

           printfn "Report: %s" iname;

           let lst_str ppf out =
             Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
               (fun ppf -> Format.fprintf ppf "%S")
               ppf out
           in
           printfn "llvm %s: %s %a" (Arch.to_string llvm_arch) iname lst_str
             llvm_opers;
           printfn "sail %s: %s %a" (Arch.to_string sail_arch) iname lst_str
             sail_opers;

           let f opers xs ppf =
             xs
             |> List.map (fun oper ->
                    match List.find_index (String.equal oper) opers with
                    | Some i -> (oper, i)
                    | None ->
                        printfn ppf oper;
                        (oper, -1))
             |> List.sort (fun (_, i1) (_, i2) -> compare i2 i1)
           in
           let llvm_ins = f llvm_opers llvm_ins "llvm: %s - implicit in" in
           let llvm_outs = f llvm_opers llvm_outs "llvm: %s - implicit out" in
           let sail_ins = f sail_opers sail_ins "sail: %s - implicit in" in
           let sail_outs = f sail_opers sail_outs "sail: %s - implicit out" in

           if List.length sail_opers <> List.length llvm_opers then
             printfn "Different number of operands"
           else (
             (try
                List.iter2
                  (fun (_, llvm_i) (_, sail_i) ->
                    if llvm_i <> sail_i then
                      printfn "Different outs (%d, %d)" llvm_i sail_i)
                  llvm_outs sail_outs
              with
             | Invalid_argument _ -> printfn "Different number of outs"
             | _ -> ());

             try
               List.iter2
                 (fun (_, llvm_i) (_, sail_i) ->
                   if llvm_i <> sail_i then
                     printfn "Different inputs (%d, %d)" llvm_i sail_i)
                 llvm_ins sail_ins
             with
             | Invalid_argument _ -> printfn "Different number of inputs"
             | _ -> ());

           let f lst =
             List.map (fun (oper, i) -> Format.sprintf "(%d, %s)" i oper) lst
           in
           printfn "llvm outs: %s" (String.concat " " (f llvm_outs));
           printfn "sail outs: %s" (String.concat " " (f sail_outs));
           printfn "llvm ins: %s" (String.concat " " (f llvm_ins));
           printfn "sail ins: %s" (String.concat " " (f sail_ins));
           printfn "";

           sail_arch
         in
         let f find_opt =
           match find_opt (arch, llvm_mnemonic) with
           | Some instr -> Some (process instr)
           | None -> None
         in
         let cannot_find () =
           printfn "Couldn't find in sail %s %s\n" iname
             (Arch.to_string llvm_arch)
         in
         match llvm_arch with
         | RV32 -> (
             match f Sail_info_RV32.find_opt with
             | Some _ -> ()
             | None -> cannot_find ())
         | RV64 -> (
             match f Sail_info_RV64.find_opt with
             | Some _ -> ()
             | None -> cannot_find ())
         | RV32_RV64 -> (
             match f Sail_info_RV32.find_opt with
             | Some sail_arch -> (
                 match sail_arch with
                 | RV32_RV64 -> ()
                 | _ -> (
                     match f Sail_info_RV64.find_opt with
                     | Some _ -> ()
                     | None -> cannot_find ()))
             | None -> ()))
