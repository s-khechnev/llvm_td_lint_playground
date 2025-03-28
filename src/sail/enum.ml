open Myast
open Core

type cfg = {
  mutable input : string;
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
}

let config = { input = ""; ocaml_code = ""; ocaml_ident = "" }

let dump_enums () =
  let asts =
    let json =
      In_channel.with_open_text config.input Yojson.Safe.from_channel
    in
    match json with
    | `List xs ->
        List.filter_map
          (fun j ->
            match Myast.def_of_yojson Myast.tannot_of_yojson j with
            | Result.Error err ->
                Format.eprintf "Error: %s\n%!" err;
                exit 1
            | Ok ast -> Some ast)
          xs
    | _ -> assert false
  in

  let hashtbl = Hashtbl.create 150 in
  List.iter
    (function
      | DEF_aux
          (DEF_type (TD_aux (TD_enum (Id_aux (Id enum_id, _), ids, _), _)), _)
        ->
          let ids =
            List.fold_left
              (fun acc -> function
                | Id_aux (Id id, _) -> id :: acc
                | _ -> assert false)
              [] ids
          in
          Hashtbl.add hashtbl enum_id ids
      | _ -> ())
    asts;

  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 150 in@]@ ";

      let out_str ppf out =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          (fun ppf -> Format.fprintf ppf "%S")
          ppf out
      in

      hashtbl
      |> Hashtbl.iter (fun key v ->
             printf "@[Hashtbl.add ans \"%s\" [%a];@]@," key out_str v);
      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find_opt = Hashtbl.find_opt %s@]@\n" config.ocaml_ident;
      printf "@[let find_exn = Hashtbl.find %s@]@\n" config.ocaml_ident;
      printf "@[let mem = Hashtbl.mem %s @]@\n%!" config.ocaml_ident)

let () =
  Arg.parse
    [
      ("-dump-enums", Arg.String (fun s -> config.input <- s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun s -> Utils.failwithf "Bad argument: %S" s)
    "";
  assert (config.input <> "");
  assert (config.ocaml_ident <> "");
  assert (config.ocaml_code <> "");
  dump_enums ()
