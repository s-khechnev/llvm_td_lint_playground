open Libsail

let default_filename = "riscv.sail.json"

include struct
  type ast = Type_check.tannot Libsail.Ast.def
end

let unmarshall ?(filename = default_filename) () =
  print_endline __FUNCTION__;
  let ast : Type_check.tannot Ast.def list =
    In_channel.with_open_bin filename (fun ch -> Marshal.from_channel ch)
  in
  ast

let save ?(filename = default_filename) ast =
  print_endline __FUNCTION__;

  let _ : Type_check.tannot Ast_defs.ast = ast in

  Out_channel.with_open_bin filename (fun ch ->
      let j =
        `List (List.map (Myast.def_to_yojson Myast.tannot_to_yojson) ast.defs)
      in
      Yojson.Safe.pretty_to_channel ch j)
