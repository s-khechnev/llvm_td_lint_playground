type cfg = { mutable filename : string; mutable outfile : string }

let cfg = { filename = ""; outfile = "" }

let read_td_json filename =
  let j = In_channel.with_open_text filename Yojson.Safe.from_channel in
  match j with `Assoc xs -> xs | _ -> exit 1

let is_bad_LLVM_key = function
  | "!instanceof" | "!tablegen_json_version" -> true
  | s when String.starts_with ~prefix:"AMDGPU" s -> true
  | s when String.starts_with ~prefix:"anonymous" s -> true
  | s when String.starts_with ~prefix:"int_x86_" s -> true
  | s when String.starts_with ~prefix:"int_arm_" s -> true
  | s when String.starts_with ~prefix:"int_aarch64" s -> true
  | s when String.starts_with ~prefix:"int_hexagon_" s -> true
  | s when String.starts_with ~prefix:"int_loongarch_" s -> true
  | _ -> false

let file_keys () =
  let xs = read_td_json cfg.filename in
  let key_count = ref 0 in
  Out_channel.with_open_text cfg.outfile (fun outch ->
      List.iteri
        (fun i (name, _) ->
          if not (is_bad_LLVM_key name) then (
            if i > 0 then Printf.fprintf outch " ";
            incr key_count;
            Printf.fprintf outch "%s.json" name))
        xs);
  Printf.printf "Keys generated: %d\n" !key_count
(*
let has_instruction_class = function
|  *)

let split_to () =
  let xs = read_td_json cfg.filename in
  let key_count = ref 0 in
  ListLabels.iter xs ~f:(fun (key, v) ->
      if not (is_bad_LLVM_key key) then
        let outfile = Printf.sprintf "./%s.json" key in
        Out_channel.with_open_text outfile (fun outch ->
            incr key_count;
            Yojson.Safe.pretty_to_channel outch v));
  Printf.printf "JSONs generated: %d\n" !key_count

let () =
  Arg.parse
    [
      ("-get-keys", Arg.Unit file_keys, "");
      ("-split-by-keys", Arg.Unit split_to, "");
      ("-o", Arg.String (fun s -> cfg.outfile <- s), "");
    ]
    (fun s -> cfg.filename <- s)
    ""
