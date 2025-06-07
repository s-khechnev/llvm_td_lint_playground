open Checker_core
open Utils
open Instruction

let not_found_in_sail = ref []

let () =
  printfn "(* To find discrepancies, do search by the word `discrepancies` *)\n";
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

           let f opers xs =
             xs
             |> List.map (fun oper ->
                    match List.find_index (String.equal oper) opers with
                    | Some i -> (i, oper)
                    | None -> (-1, oper))
             |> List.sort (fun (i1, s1) (i2, s2) ->
                    if i1 = -1 && i2 = -1 then String.compare s2 s1
                    else compare i2 i1)
           in
           let llvm_ins = f llvm_opers llvm_ins in
           let llvm_outs = f llvm_opers llvm_outs in
           let sail_ins = f sail_opers sail_ins in
           let sail_outs = f sail_opers sail_outs in

           let opers_to_str xs =
             String.concat " "
               (List.map
                  (fun (i, oper) ->
                    if i = -1 then Format.sprintf "(implicit: %s)" oper
                    else Format.sprintf "(%d: %s)" i oper)
                  xs)
           in

           let pp ppf xs =
             if List.is_empty xs then () else printfn ppf (opers_to_str xs)
           in
           pp "llvm outs: %s" llvm_outs;
           pp "sail outs: %s" sail_outs;
           pp "llvm ins: %s" llvm_ins;
           pp "sail ins: %s" sail_ins;

           printfn "llvm: mayLoad = %B" llvm_mayLoad;
           printfn "sail: mayLoad = %B" sail_mayLoad;

           printfn "llvm: mayStore = %B" llvm_mayStore;
           printfn "sail: mayStore = %B" sail_mayStore;

           let differences = Queue.create () in
           let check_opers llvm_xs sail_xs msg =
             let get_diffs xs1 xs2 =
               List.filter
                 (fun (i1, _) -> not (List.exists (fun (i2, _) -> i2 = i1) xs2))
                 xs1
             in
             let llvm_diffs = get_diffs llvm_xs sail_xs in
             let sail_diffs = get_diffs sail_xs llvm_xs in

             if List.is_empty llvm_diffs then ()
             else
               Queue.add
                 (Format.sprintf "in llvm %s: %s" msg (opers_to_str llvm_diffs))
                 differences;

             if List.is_empty sail_diffs then ()
             else
               Queue.add
                 (Format.sprintf "in sail %s: %s" msg (opers_to_str sail_diffs))
                 differences
           in

           if List.length llvm_opers <> List.length sail_opers then
             Queue.add "number of operands" differences
           else (
             check_opers llvm_outs sail_outs "outs";
             check_opers llvm_ins sail_ins "ins");

           if llvm_mayLoad <> sail_mayLoad then
             Queue.add "diff mayLoad" differences;
           if llvm_mayStore <> sail_mayStore then
             Queue.add "diff mayStore" differences;

           if Queue.is_empty differences then ()
           else (
             printfn "%s discrepancies:" iname;
             Queue.iter (printfn "  %s") differences);

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
