open Core.Utils
open Myast_iterator
open Myast

type cfg = {
  mutable sail_json : string;
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config =
  { sail_json = ""; ocaml_code = ""; ocaml_ident = ""; dot_file = "graph.dot" }

let is_name_for_tracing = function "" -> true | _ -> false

let sail_json =
  lazy
    (match
       In_channel.with_open_text config.sail_json Yojson.Safe.from_channel
     with
    | `List xs ->
        List.map
          (fun j ->
            match def_of_yojson tannot_of_yojson j with
            | Result.Ok def -> def
            | _ -> assert false)
          xs
    | _ -> assert false)

let funcs : (string, string list * Libsail.Type_check.tannot exp) Hashtbl.t =
  Hashtbl.create 2000

let aliases : (string, (string * string) list) Hashtbl.t = Hashtbl.create 500
let executes : (string, unit) Hashtbl.t = Hashtbl.create 500

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
                          (P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)), _),
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
                          (P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)), _),
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
                          (P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)), _),
                        E_aux
                          ( E_app
                              ( Id_aux (Id "add_bits", _),
                                [
                                  E_aux (E_id (Id_aux (Id arg, _)), _);
                                  E_aux (E_app (Id_aux (Id "to_bits", _), _), _);
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

exception ReachTranslateAddr

let get_may_loads g =
  let result = Hashtbl.create 50 in
  let stop_flag = ref false in
  let on_edge (_, _, v_dst) =
    if String.equal v_dst "translateAddr" then stop_flag := true;

    if !stop_flag then ()
    else if Hashtbl.mem executes v_dst then Hashtbl.add result v_dst ()
  in
  Call_graph.dfs g ~start_v:"read_ram" ~on_edge;
  result

let get_may_stores g =
  let result = Hashtbl.create 50 in
  let stop_flag = ref false in
  let on_edge (_, _, v_dst) =
    if String.equal v_dst "translateAddr" then stop_flag := true;

    if !stop_flag then ()
    else if Hashtbl.mem executes v_dst then Hashtbl.add result v_dst ()
  in
  Call_graph.dfs g ~start_v:"write_ram" ~on_edge;
  result

let dump_execute () =
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
                    Format.printf "%a" (pp_pat_aux pp_tannot) paux;
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
                      printfn "@[%s: %a@]@," id (pp_exp pp_tannot) body;

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
                      | Some s -> Format.sprintf "%s %s" id s
                      | None -> id
                    in

                    Hashtbl.add funcs id (args, body);
                    Hashtbl.add executes id ()
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id id, Range ({ pos_fname = path; _ }, _)),
                          Pat_aux (Pat_exp (P_aux (paux, _), body), _) ),
                      _ )
                  when String.starts_with path ~prefix:"../../sail-riscv" ->
                    Hashtbl.add funcs id (extract_args paux, body)
                | _ -> ())
              funcls
        | _ -> ())
      (Lazy.force sail_json)
  in

  let g = Call_graph.generate funcs in

  let () =
    let cur_aliases = Hashtbl.create 10 in
    Hashtbl.iter
      (fun name (args, body) ->
        Hashtbl.clear cur_aliases;
        collect_aliases body (fun alias arg ->
            if List.mem arg args then Hashtbl.add cur_aliases alias arg
            else if Hashtbl.mem cur_aliases arg then
              Hashtbl.replace cur_aliases alias arg);

        Hashtbl.add aliases name (List.of_seq (Hashtbl.to_seq cur_aliases));

        if debug then (
          printfn "Aliases for %s" name;
          Hashtbl.iter (printfn "%s -> %s") cur_aliases))
      funcs
  in

  Call_graph.dump g config.dot_file;

  let outs =
    Call_graph.propogate_operands ~g ~aliases
      (List.to_seq [ ("wX", [ "r" ]); ("wF", [ "r" ]); ("wV", [ "r" ]) ])
  in
  let ins =
    Call_graph.propogate_operands ~g ~aliases
      (List.to_seq [ ("rX", [ "r" ]); ("rF", [ "r" ]); ("rV", [ "r" ]) ])
  in

  let mayLoads = get_may_loads g in
  let mayStores = get_may_stores g in

  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[open Core.Instruction@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 1000 in@]@ ";

      let open Assembly_helper in
      Mnemonics.mnemonics
      |> Hashtbl.iter (fun mnemonic (sail_name, opers) ->
             let sail_id =
               match sail_name with
               | IK_straight s -> s
               | IK_singledef (a, b) -> Format.sprintf "%s %s" a b
             in
             let ins =
               let imm_ins =
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
                     opers
                 with
                 | xs when not (String.equal "VMVRTYPE" sail_id) -> xs
                 | _ -> []
               in
               let in_regs =
                 match Hashtbl.find_opt ins sail_id with
                 | Some ins -> ins
                 | None -> []
               in
               in_regs @ imm_ins
             in
             let outs =
               match Hashtbl.find_opt outs sail_id with
               | Some outs -> outs
               | None -> []
             in
             let mayLoad = Hashtbl.mem mayLoads sail_id in
             let mayStore = Hashtbl.mem mayStores sail_id in
             Core.Utils.printf_add_instr ppf
               ({ mnemonic; operands = opers; ins; outs; mayLoad; mayStore }
                 : Core.Instruction.t));

      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find = Hashtbl.find %s @]@," config.ocaml_ident;
      printf "@[let find_opt = Hashtbl.find_opt %s @]@," config.ocaml_ident;
      printf "@[let mem = Hashtbl.mem %s @]@," config.ocaml_ident)

let () =
  Arg.parse
    [
      ("-dump-execute", Arg.String (fun s -> config.sail_json <- s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun s -> failwithf "Bad argument: %S" s)
    "";
  dump_execute ()
