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
      exit 0)
    ""
