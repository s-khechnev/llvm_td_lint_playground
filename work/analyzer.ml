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

let is_good_suffix = function 'B' | 'W' | 'D' | 'H' -> true | _ -> false

let is_good_suffix_s = function
  | "UW" | "B_AQ" | "B_AQ_RL" | "B_RL" | "D_AQ" | "D_AQ_RL" | "D_RL" | "H_AQ"
  | "H_AQ_RL" | "H_RL" | "W_AQ" | "W_AQ_RL" | "W_RL" | "INX" | "IN32X" ->
      (* TODO: Understand which if this suffixes are about *)
      true
  | _ -> false

let smart_rewrite iname =
  let len = String.length iname in
  match String.index iname '_' with
  | exception Not_found -> None
  | c when len - 2 = c && is_good_suffix iname.[c + 1] ->
      Some (String.sub iname 0 c)
  | c when is_good_suffix_s (String.sub iname (c + 1) (len - c - 1)) ->
      Some (String.sub iname 0 c)
  | _ -> None

open Format

let%expect_test _ =
  let iname = "ADD_UW" in
  let len = String.length iname in
  printf "len = %d, _pos = %d\n" len (String.index iname '_');
  let ans = smart_rewrite iname in
  printf "%a\n%!" Format.(pp_print_option pp_print_string) ans;
  [%expect {|
    len = 6, _pos = 3
    ADD |}]
