type analyzer_config = {
  mutable libsail_path : string;
  mutable input_files : string list;
}

let config = { libsail_path = ""; input_files = [] }

include struct
  open Libsail

  let register_default_target () =
    Target.register ~name:"default" (fun _ _ _ _ _ _ -> ())

  let load_plugin opts plugin =
    Printf.printf "%s %s\n" __FUNCTION__ plugin;
    try
      Dynlink.loadfile_private plugin;
      opts := Arg.align (!opts @ Target.extract_options ())
    with Dynlink.Error msg ->
      prerr_endline
        ("Failed to load plugin " ^ plugin ^ ": " ^ Dynlink.error_message msg)

  let options = ref []

  let () =
    match Libsail_sites.Sites.plugins with
    | [] -> ()
    | dir :: _ ->
        List.iter
          (fun plugin ->
            let path = Filename.concat dir plugin in

            if Filename.extension plugin = ".cmxs" then load_plugin options path)
          (Array.to_list (Sys.readdir dir))

  let test1 () =
    let _ =
      try
        Frontend.load_files
          ~target:(register_default_target ())
          "/home/kakadu/mand/sail/sail" [] Type_check.initial_env
          (List.rev config.input_files)
      with Reporting.Fatal_error e ->
        Reporting.print_error e;
        exit 1
    in
    print_endline "OK"
end

let () =
  Arg.parse
    (!options
    @ [
        ("-test1", Arg.Unit test1, "");
        ( "-analyzer-path",
          Arg.String (fun s -> config.libsail_path <- s),
          " Set location for libsail .sail files" );
      ])
    (fun filename -> config.input_files <- filename :: config.input_files)
    "help"
