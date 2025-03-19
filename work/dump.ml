[@@@ocaml.warnerror "-unused-extension"]

let failwithf fmt = Format.kasprintf failwith fmt
let sprintf fmt = Printf.sprintf fmt

let log fmt =
  if true then Format.kasprintf print_endline fmt
  else Format.ikfprintf (fun _ -> ()) Format.std_formatter fmt

let loge fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt

type cfg = {
  mutable sail_json : string;
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config =
  { sail_json = ""; ocaml_code = ""; ocaml_ident = ""; dot_file = "graph1.dot" }

let printfn ppf = Format.kasprintf print_endline ppf

let sail_json =
  lazy
    (match
       In_channel.with_open_text config.sail_json Yojson.Safe.from_channel
     with
    | `List xs ->
        List.map
          (fun j ->
            match Myast.def_of_yojson Myast.tannot_of_yojson j with
            | Result.Ok def -> def
            | _ -> assert false)
          xs
    | _ -> assert false)

let funcs :
    (string, string list * Libsail.Type_check.tannot Libsail.Ast.exp) Hashtbl.t
    =
  Hashtbl.create 10000

let executes = Hashtbl.create 10000
let is_name_for_tracing = function "" -> true | _ -> false

module Collect_out_info = struct
  open Myast_iterator
  open Myast

  let aliases : (string, (string * string) list) Hashtbl.t = Hashtbl.create 500

  let collect_aliases body add_alias =
    let it =
      {
        default_iterator with
        exp_aux =
          (fun self e ->
            (match e with
            | E_let
                ( LB_aux
                    ( LB_val
                        ( P_aux (P_id (Id_aux (Id alias, _)), _),
                          E_aux
                            ( E_app
                                ( Id_aux (Id "creg2reg_idx", _),
                                  [ E_aux (E_id (Id_aux (Id arg, _)), _) ] ),
                              _ ) ),
                      _ ),
                  _ )
            | E_let
                ( LB_aux
                    ( LB_val
                        ( P_aux
                            ( P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)),
                              _ ),
                          E_aux
                            ( E_app
                                ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                                  [ _; E_aux (E_id (Id_aux (Id arg, _)), _) ] ),
                              _ ) ),
                      _ ),
                  _ ) ->
                add_alias alias arg
            | E_let
                ( LB_aux
                    ( LB_val
                        ( P_aux
                            ( P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)),
                              _ ),
                          E_aux
                            ( E_app
                                ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                                  [
                                    _;
                                    E_aux
                                      ( E_app
                                          (Id_aux (Id "bitvector_concat", _), xs),
                                        _ );
                                  ] ),
                              _ ) ),
                      _ ),
                  _ ) ->
                let arg =
                  Option.get
                  @@ List.find_map
                       (function
                         | E_aux (E_id (Id_aux (Id id, _)), _) -> Some id
                         | _ -> None)
                       xs
                in
                add_alias alias arg
            | E_let
                ( LB_aux
                    ( LB_val
                        ( P_aux
                            ( P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)),
                              _ ),
                          E_aux
                            ( E_app
                                ( Id_aux (Id "add_bits", _),
                                  [
                                    E_aux (E_id (Id_aux (Id arg, _)), _);
                                    E_aux
                                      (E_app (Id_aux (Id "to_bits", _), _), _);
                                  ] ),
                              _ ) ),
                      _ ),
                  _ ) ->
                add_alias alias arg
            | _ -> ());
            default_iterator.exp_aux self e);
      }
    in
    it.exp it body
end

let dump_execute () =
  let open Myast in
  let execute_ids = Queue.create () in
  let () =
    List.iter
      (function
        | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
            let extract_args paux =
              let rec helper = function
                | P_id (Id_aux (Id id, _)) -> id
                | P_wild -> "_"
                | P_lit (L_aux (L_hex n, _)) -> "0x" ^ n
                | P_lit (L_aux (L_unit, _)) -> "unit"
                | P_struct _ -> "P_struct"
                | P_var (P_aux (paux, _), _) -> helper paux
                | P_typ (_, P_aux (paux, _)) -> helper paux
                | _ ->
                    Format.printf "%a" (Myast.pp_pat_aux Myast.pp_tannot) paux;
                    failwithf "Arg's extraction not implemented\n"
              in
              match paux with
              | P_tuple args ->
                  List.map (function P_aux (paux, _) -> helper paux) args
              | p -> [ helper p ]
            in

            List.iter
              (function
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id "execute", _),
                          Pat_aux
                            ( Pat_exp
                                ( P_aux
                                    ( P_app
                                        (Id_aux (Id id, _), [ P_aux (parg, _) ]),
                                      _ ),
                                  body ),
                              _ ) ),
                      _ ) ->
                    if is_name_for_tracing id then
                      printfn "@[%s: %a@]@," id (pp_exp Myast.pp_tannot) body;

                    let args = extract_args parg in
                    let id =
                      let enum_arg =
                        List.find_map
                          (fun s ->
                            if Char.uppercase_ascii s.[0] = s.[0] then Some s
                            else None)
                          args
                      in
                      match enum_arg with
                      | Some s -> Format.sprintf "%s____%s" id s
                      | None -> id
                    in

                    Hashtbl.add funcs id (args, body);
                    Hashtbl.add executes id ();
                    Queue.add (id, args, body) execute_ids
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id id, Range ({ pos_fname = path; _ }, _)),
                          Pat_aux (Pat_exp (P_aux (paux, _), body), _) ),
                      _ )
                  when String.starts_with path ~prefix:"../sail-riscv" ->
                    Hashtbl.add funcs id (extract_args paux, body)
                | _ -> ())
              funcls
        | _ -> ())
      (Lazy.force sail_json)
  in

  let g = Call_graph.generate funcs in

  let () =
    Hashtbl.iter
      (fun name (args, body) ->
        let aliases = Hashtbl.create 10 in
        Collect_out_info.collect_aliases body (fun alias arg ->
            if List.mem arg args then Hashtbl.add aliases alias arg
            else if Hashtbl.mem aliases arg then
              Hashtbl.replace aliases alias arg);

        (* printfn "Aliases for %s" name;
           Hashtbl.iter (printfn "%s -> %s") aliases; *)
        Hashtbl.add Collect_out_info.aliases name
          (List.of_seq (Hashtbl.to_seq aliases)))
      funcs
  in

  Call_graph.dump g config.dot_file;

  let outs =
    Call_graph.propogate_operands ~g ~aliases:Collect_out_info.aliases
      (List.to_seq [ ("wX", [ "r" ]); ("wF", [ "r" ]); ("wV", [ "r" ]) ])
  in

  let ins =
    Call_graph.propogate_operands ~g ~aliases:Collect_out_info.aliases
      (List.to_seq [ ("rX", [ "r" ]); ("rF", [ "r" ]); ("rV", [ "r" ]) ])
  in

  Queue.iter
    (fun (id, args, _) ->
      let in_args =
        match
          List.find_all
            (fun id ->
              String.ends_with ~suffix:"imm" id
              || List.mem id
                   [
                     "nzi";
                     "bs";
                     "shamt";
                     "rnum";
                     "rm";
                     "constantidx";
                     "vm";
                     "pred";
                     "succ";
                   ])
            args
        with
        | xs when not (String.equal "VMVRTYPE" id) -> xs
        | _ -> []
      in
      match Hashtbl.find_opt ins id with
      | Some dst_ins -> Hashtbl.replace ins id (dst_ins @ in_args)
      | None -> Hashtbl.add ins id in_args)
    execute_ids;

  Hashtbl.iter
    (fun exec _ ->
      (* print_endline exec; *)
      (match Hashtbl.find_opt ins exec with
      | Some opers_g -> (
          try
            let from61 = From6159.lookup_exn exec in
            let _, ins =
              match from61 with
              | From6159.CI_default _, info -> (info.out, info.inputs)
              | From6159.CI_hacky (_, _), info -> (info.out, info.inputs)
            in
            if not @@ List.for_all (fun oper -> List.mem oper opers_g) ins then (
              printfn "Diff ins for: %s" exec;

              printfn "from6159: %s" (String.concat " " ins);
              printfn "G: %s" (String.concat " " opers_g))
          with Not_found ->
            if not @@ String.starts_with exec ~prefix:"F_" then
              printfn "Not found from6159 %s" exec)
      | _ -> ());
      match Hashtbl.find_opt outs exec with
      | Some opers_g -> (
          try
            let from61 = From6159.lookup_exn exec in
            let outs, _ =
              match from61 with
              | From6159.CI_default _, info -> (info.out, info.inputs)
              | From6159.CI_hacky (_, _), info -> (info.out, info.inputs)
            in
            if not @@ List.for_all (fun oper -> List.mem oper opers_g) outs then (
              printfn "Diff outs for: %s" exec;

              printfn "from6159: %s" (String.concat " " outs);
              printfn "G: %s" (String.concat " " opers_g))
          with Not_found ->
            if not @@ String.starts_with exec ~prefix:"F_" then
              printfn "Not found from6159 %s" exec)
      | _ -> ())
    executes;

  Out_channel.with_open_text "out.ml" (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[include From6159_helper@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hash_info.create 1000 in@]@ ";

      outs
      |> Hashtbl.iter (fun key v ->
             let out_str ppf out =
               Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
                 (fun ppf -> Format.fprintf ppf "%S")
                 ppf out
             in
             printf "@[def ans  %S {out=[%a]};@]@," key out_str v);

      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let lookup_exn = Hash_info.find %s @]@," config.ocaml_ident;
      printf "@[let mem = Hash_info.mem %s @]@," config.ocaml_ident)

let () =
  Arg.parse
    [
      ( "-dump-execute",
        Arg.String
          (fun s ->
            config.sail_json <- s;
            dump_execute ()),
        "" );
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun s -> failwithf "Bad argument: %S" s)
    ""
