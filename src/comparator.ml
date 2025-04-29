open Checker_core
open Utils

let () =
  Llvm_info.llvm_info |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (m1, _) (m2, _) -> compare m1 m2)
  |> List.iter (fun (iname, llvm_instr) ->
         let {
           mnemonic = llvm_mnemonic;
           operands = llvm_opers;
           ins = llvm_ins;
           outs = llvm_outs;
           mayLoad = llvm_mayLoad;
           mayStore = llvm_mayStore;
         } : Instruction.t =
           llvm_instr
         in
         let llvm_mnemonic =
           if String.ends_with llvm_mnemonic ~suffix:"aqrl" then
             let iname = Utils.chop_suffix ~suffix:"rl" llvm_mnemonic in
             iname ^ ".rl"
           else llvm_mnemonic
         in
         match Sail_info.find_opt llvm_mnemonic with
         | Some sail_instr ->
             let {
               mnemonic = _;
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
             printfn "llvm: %s %a" iname lst_str llvm_opers;
             printfn "sail: %s %a" iname lst_str sail_opers;

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

             printfn "llvm %s: mayLoad = %B" iname llvm_mayLoad;
             printfn "sail %s: mayLoad = %B" iname sail_mayLoad;
             if llvm_mayLoad <> sail_mayLoad then printfn "Different mayLoad";

             printfn "llvm %s: mayStore = %B" iname llvm_mayStore;
             printfn "sail %s: mayStore = %B" iname sail_mayStore;
             if llvm_mayStore <> sail_mayStore then printfn "Different mayStore";

             printfn ""
         | None -> ())
