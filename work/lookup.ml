type config = { mutable instructions_file : string }

let config = { instructions_file = "./json/allkeys.txt" }

type stats = {
  mutable from6159 : int;
  mutable total : int;
  mutable first_unknown : string list;
}

let stats = { from6159 = 0; total = 0; first_unknown = [] }

let save_unknown orig s =
  let s = if orig <> s then Printf.sprintf "%s(%s)" s orig else s in
  match stats.first_unknown with
  | [] -> stats.first_unknown <- [ s ]
  | [ a ] -> stats.first_unknown <- [ a; s ]
  | [ a; b ] -> stats.first_unknown <- [ a; b; s ]
  | [ a; b; c ] -> stats.first_unknown <- [ a; b; c; s ]
  | _ -> ()

let report () =
  let open Format in
  printf "Total instructions: %d\n" stats.total;
  printf "from6159: %d\n" stats.from6159;
  printf "First unknown: %s\n" (String.concat " " stats.first_unknown)

let omitted_explicitly = [ ""; "ANNOTATION_LABEL" ]

let is_omitted_explicitly s =
  (* AMOADD_D* and AMOADD_B* not found in sail *)
  String.starts_with ~prefix:"AMOADD_" s
  || String.starts_with ~prefix:"AMOAND_" s
  || String.starts_with ~prefix:"AMOCAS_" s
  || List.mem s omitted_explicitly

let () =
  let all_iname =
    In_channel.with_open_text config.instructions_file In_channel.input_all
    |> String.split_on_char '\n'
  in
  stats.total <- List.length all_iname;
  ListLabels.iter all_iname ~f:(fun iname ->
      let mangled_iname =
        match iname with
        | "ADD_UW" -> "ADDUW"
        | _ -> (
            match Analyzer.smart_rewrite iname with
            | Some x -> x
            | None -> iname)
      in
      let () =
        match iname with
        | s when is_omitted_explicitly s -> ()
        | _ ->
            if List.mem mangled_iname From6159.from6159 then
              stats.from6159 <- stats.from6159 + 1
            else if
              List.mem ("RISCV_" ^ mangled_iname) From6159.from6159_hacky
              || List.mem mangled_iname From6159.from6159_hacky
            then stats.from6159 <- stats.from6159 + 1
            else save_unknown iname mangled_iname
      in
      ());
  report ()
