let post_process ast =
  let (_ : Libsail.Type_check.tannot Myast.def list) = ast in
  ast
  |> List.iteri (fun n def ->
         Out_channel.with_open_text (Printf.sprintf "tmp/%05d.json" n)
           (fun ch ->
             let j = Myast.def_to_yojson Myast.tannot_to_yojson def in
             Yojson.Safe.pretty_to_channel ch j);
         Out_channel.with_open_text (Printf.sprintf "tmp/%05d.txt" n) (fun ch ->
             Myast.pp_def Myast.pp_tannot
               (Format.formatter_of_out_channel ch)
               def);

         ());
  ()

let () =
  Arg.parse []
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
      post_process ast;
      exit 0)
    ""
