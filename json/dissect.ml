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
  | s when String.starts_with ~prefix:"Pseudo" s -> true
  | _ -> false

let is_bad_JSON_data _ : Yojson.Safe.t -> bool =
 fun j ->
  match j with
  | `Assoc xs -> (
      match List.assoc "!superclasses" xs with
      | `List classes ->
          let ans =
            List.mem (`String "Pseudo") classes
            || List.mem (`String "StandardPseudoInstruction") classes
          in
          (* if ans then
             Printf.printf "Key %S filtered because of Pseudo class\n" key; *)
          ans
      | exception Not_found -> false
      | _ -> false)
  | _ -> false

let file_keys () =
  let xs = read_td_json cfg.filename in
  let key_count = ref 0 in
  Out_channel.with_open_text cfg.outfile (fun outch_text ->
      List.iter
        (fun (name, jdata) ->
          if is_bad_LLVM_key name || is_bad_JSON_data name jdata then ()
          else (
            incr key_count;
            Printf.fprintf outch_text "%s\n" name))
        xs);
  Printf.printf "Instructions number in LLVM after filtering: %d\n" !key_count

let () =
  Arg.parse
    [
      ("-get-keys", Arg.Unit file_keys, "");
      ("-o", Arg.String (fun s -> cfg.outfile <- s), "");
    ]
    (fun s -> cfg.filename <- s)
    ""
