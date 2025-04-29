open Checker_core
open Utils
open Instruction

let not_found_in_sail = ref []

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
           mayLoad = llvm_mayLoad;
           mayStore = llvm_mayStore;
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
             mayLoad = sail_mayLoad;
             mayStore = sail_mayStore;
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
                  (fun (llvm_reg, llvm_i) (sail_reg, sail_i) ->
                    let prr_err () =
                      printfn "Different outs (%d, %d)" llvm_i sail_i
                    in
                    if llvm_i = -1 && sail_i = -1 && llvm_reg <> sail_reg then
                      prr_err ()
                    else if llvm_i <> sail_i then prr_err ())
                  llvm_outs sail_outs
              with
             | Invalid_argument _ -> printfn "Different number of outs"
             | _ -> ());

             try
               List.iter2
                 (fun (llvm_reg, llvm_i) (sail_reg, sail_i) ->
                   let prr_err () =
                     printfn "Different ins (%d, %d)" llvm_i sail_i
                   in
                   if llvm_i = -1 && sail_i = -1 && llvm_reg <> sail_reg then
                     prr_err ()
                   else if llvm_i <> sail_i then prr_err ())
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

           printfn "llvm %s: mayLoad = %B" iname llvm_mayLoad;
           printfn "sail %s: mayLoad = %B" iname sail_mayLoad;
           if llvm_mayLoad <> sail_mayLoad then printfn "Different mayLoad";

           printfn "llvm %s: mayStore = %B" iname llvm_mayStore;
           printfn "sail %s: mayStore = %B" iname sail_mayStore;
           if llvm_mayStore <> sail_mayStore then printfn "Different mayStore";

           printfn ""
         in

         let cannot_find i = not_found_in_sail := i :: !not_found_in_sail in
         let llvm_instr = (arch, llvm_mnemonic) in
         match llvm_arch with
         | RV32 -> (
             match Sail_info_RV32.find_opt llvm_instr with
             | Some i -> process i
             | None -> cannot_find llvm_instr)
         | RV64 -> (
             match Sail_info_RV64.find_opt llvm_instr with
             | Some i -> process i
             | None -> cannot_find llvm_instr)
         | RV32_RV64 -> (
             match
               ( Sail_info_RV32.find_opt llvm_instr,
                 Sail_info_RV64.find_opt llvm_instr )
             with
             | None, Some i -> process i
             | Some i, None -> process i
             | Some i32, Some i64 ->
                 if Instruction.equal i32 i64 then process i32
                 else (
                   process i32;
                   process i64)
             | _ -> cannot_find llvm_instr));

  printfn "Not found in sail: %s"
    (String.concat " "
       (List.map (fun (_, i) -> Format.sprintf "%s" i) !not_found_in_sail))
