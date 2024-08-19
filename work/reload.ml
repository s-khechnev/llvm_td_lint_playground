let post_process ast =
  let (_ : Libsail.Type_check.tannot Myast.def list) = ast in
  ast
  |> List.iteri (fun n def ->
         let file = Printf.sprintf "./%05d.json" n in
         if Sys.file_exists file then Sys.remove file;
         Out_channel.with_open_text file (fun ch ->
             let j = Myast.def_to_yojson Myast.tannot_to_yojson def in
             let fmt = Format.formatter_of_out_channel ch in
             Format.pp_set_max_indent fmt 2000;
             Yojson.Safe.pretty_print fmt j;
             Format.pp_print_flush fmt ());
         let file = Printf.sprintf "./%05d.txt" n in
         if Sys.file_exists file then Sys.remove file;
         Out_channel.with_open_text file (fun ch ->
             Myast.pp_def Myast.pp_tannot
               (Format.formatter_of_out_channel ch)
               def);

         ())

let dump_execute jfile =
  let _ast =
    let j = In_channel.with_open_text jfile Yojson.Safe.from_channel in
    match Myast.def_of_yojson Myast.tannot_of_yojson j with
    | Result.Error err ->
        Format.eprintf "Error: %s\n%!" err;
        exit 1
    | Ok ast -> ast
  in

  ()

let () =
  Arg.parse
    [
      ( "-dump-from",
        Arg.String
          (fun s ->
            let j = In_channel.with_open_text s Yojson.Safe.from_channel in
            let ast =
              match j with
              | `List xs ->
                  List.map
                    (fun j ->
                      match Myast.def_of_yojson Myast.tannot_of_yojson j with
                      | Result.Error err ->
                          Format.eprintf "Error: %s\n%!" err;
                          exit 1
                      | Ok ast -> ast)
                    xs
              | _ -> assert false
            in

            let (_ : Libsail.Type_check.tannot Myast.def list) = ast in
            post_process ast),
        "" );
      ("-dump-execute", Arg.String (fun s -> dump_execute s), "");
    ]
    (fun _ -> assert false)
    ""
