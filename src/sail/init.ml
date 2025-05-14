(* generate for rv32/64 z3_problems and save pretty print sail's ast *)

let process rv_bits =
  let input_files =
    let sail_files_txt = Format.sprintf "sail_files_%s.txt" rv_bits in
    In_channel.(with_open_text sail_files_txt input_lines)
  in
  let ast, _, _ =
    Mysail.main (Array.of_list ("sail" :: "-memo_z3" :: input_files))
  in
  let pp_file_name = Format.sprintf "pp_ast_%s.txt" rv_bits in
  Myast.save pp_file_name ast

let () =
  match Sys.argv with
  | [| _ |] ->
      List.iter
        (fun xlen ->
          let cmd = Format.sprintf "%s %s" Sys.argv.(0) xlen in
          ignore (Unix.system cmd))
        [ "RV32"; "RV64" ]
  | [| _; rv_bits |] -> process rv_bits
  | _ -> assert false
