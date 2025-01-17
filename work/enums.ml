let input = "./riscv.sail.json"

open Myast

let () =
  let asts =
    let json = In_channel.with_open_text input Yojson.Safe.from_channel in
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

  let hashtbl = Hashtbl.create 3000 in
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

  Out_channel.with_open_text "enum_hashtbl.ml" (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let ans = \n  let ans = Hashtbl.create 100 in@]@ ";

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
      printf "@[let find = Hashtbl.find ans@]@\n";
      printf "@[let mem = Hashtbl.mem ans@]@\n%!")
