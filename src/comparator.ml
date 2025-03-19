open Core

let () =
  let open Utils in
  Llvm_info.llvm_info |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (m1, _) (m2, _) -> compare m1 m2)
  |> List.iter (fun (iname, (llvm_instr : Core.Instruction.t)) ->
         let f opers xs ppf =
           xs
           |> List.map (fun oper ->
                  match List.find_index (String.equal oper) opers with
                  | Some i -> (oper, i)
                  | None ->
                      printfn ppf iname oper;
                      (oper, -1))
           |> List.sort (fun (_, i1) (_, i2) -> compare i1 i2)
           |> List.rev
         in
         let {
           mnemonic = llvm_mnemonic;
           operands = llvm_opers;
           ins = llvm_ins;
           outs = llvm_outs;
         } : Core.Instruction.t =
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
             printfn "Report: %s" iname;
             let {
               mnemonic = _;
               operands = sail_opers;
               ins = sail_ins;
               outs = sail_outs;
             } : Core.Instruction.t =
               sail_instr
             in

             let lst_str ppf out =
               Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
                 (fun ppf -> Format.fprintf ppf "%S")
                 ppf out
             in
             printfn "llvm: %s %a" iname lst_str llvm_opers;
             printfn "sail: %s %a" iname lst_str sail_opers;

             let llvm_ins = f llvm_opers llvm_ins "llvm %s: %s - implicit in" in
             let llvm_outs =
               f llvm_opers llvm_outs "llvm %s: %s - implicit out"
             in
             let sail_ins = f sail_opers sail_ins "sail %s: %s - implicit in" in
             let sail_outs =
               f sail_opers sail_outs "sail %s: %s - implicit out"
             in

             if List.length sail_opers <> List.length llvm_opers then
               printfn "%s: Different number of operands" iname
             else (
               (try
                  List.iter2
                    (fun (_, llvm_i) (_, sail_i) ->
                      if llvm_i <> sail_i then
                        printfn "%s: Different outs (%d, %d)" iname llvm_i
                          sail_i)
                    llvm_outs sail_outs
                with
               | Invalid_argument _ ->
                   printfn "%s: Different number of outs" iname
               | _ -> ());

               try
                 List.iter2
                   (fun (_, llvm_i) (_, sail_i) ->
                     if llvm_i <> sail_i then
                       printfn "%s: Different inputs (%d, %d)" iname llvm_i
                         sail_i)
                   llvm_ins sail_ins
               with
               | Invalid_argument _ ->
                   printfn "%s: Different number of inputs" iname
               | _ -> ());

             let f lst =
               List.map (fun (oper, i) -> Format.sprintf "(%d, %s)" i oper) lst
             in
             printfn "llvm outs: %s" (String.concat " " (f llvm_outs));
             printfn "sail outs: %s" (String.concat " " (f sail_outs));
             printfn "llvm ins: %s" (String.concat " " (f llvm_ins));
             printfn "sail ins: %s" (String.concat " " (f sail_ins));

             printfn ""
         | None -> ())
