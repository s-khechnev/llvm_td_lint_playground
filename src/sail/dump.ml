open Core.Utils
open Myast_iterator
open Myast

type cfg = {
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config = { ocaml_code = ""; ocaml_ident = ""; dot_file = "graph.dot" }
let is_name_for_tracing = function "ITYPE" -> true | _ -> false
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
                _ ) -> (
              match
                List.find_map
                  (function
                    | E_aux (E_id (Id_aux (Id id, _)), _) -> Some id | _ -> None)
                  xs
              with
              | Some arg -> add_alias alias arg
              | None -> ())
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

let dump_execute (ast : 'a Libsail.Ast_defs.ast) _ _ =
  let funcs = Spec.FuncTable.create 2500 in
  let open Spec in
  let open Libsail in
  let open Ast_util in
  let start_time = Unix.gettimeofday () in
  let () =
    let pat_to_lst pat =
      match unaux_pat pat with
      | P_tuple pats
      | P_vector pats
      | P_list pats
      | P_vector_concat pats
      | P_string_append pats ->
          pats
      | _ -> [ pat ]
    in
    let pats_to_strs pats =
      List.map
        (fun p ->
          let rec helper pat =
            match unaux_pat pat with
            | P_var (p, _) -> helper p
            | P_typ (_, p) -> helper p
            | _ -> string_of_pat pat
          in
          helper p)
        pats
    in
    List.iter
      (function
        | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
            List.iter
              (function
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id "execute", _),
                          Pat_aux
                            ( Pat_exp
                                ( P_aux (P_app (Id_aux (Id id, _), [ parg ]), _),
                                  fbody ),
                              _ ) ),
                      _ ) -> (
                    (* if is_name_for_tracing id then
                       printfn "@[%s: %a@]@," id (pp_funcl_aux pp_tannot) exec; *)
                    let pargs = pat_to_lst parg in
                    let str_args = pats_to_strs pargs in
                    let pargsi = List.mapi (fun i p -> (i, p)) pargs in
                    let speced_args = Spec.get_speced_args pargsi in
                    let args_to_spec = Spec.get_args_to_spec pargsi in
                    let rec product = function
                      | [] -> [ [] ]
                      | (i, id, xs) :: t ->
                          let rest = product t in
                          List.concat_map
                            (fun x -> List.map (fun r -> (i, id, x) :: r) rest)
                            xs
                    in
                    match (speced_args, args_to_spec) with
                    | [], [] ->
                        FuncTable.add funcs (F_usual id) (str_args, fbody)
                    | speced_args, [] ->
                        FuncTable.add funcs
                          (F_specialized (id, speced_args))
                          (str_args, fbody)
                    | already_speced_args, args_to_spec ->
                        let ref_vars =
                          Constant_propagation.referenced_vars fbody
                        in
                        product args_to_spec
                        |> List.iter (fun xs ->
                               let speced_fbody, _ =
                                 let substs =
                                   Bindings.of_list
                                     (List.map
                                        (fun (_, id, exp) -> (id, exp))
                                        xs)
                                 in
                                 Myconstant_propagation.const_prop "" ast
                                   ref_vars (substs, KBindings.empty)
                                   Bindings.empty fbody
                               in
                               let speced =
                                 List.map (fun (i, _, exp) -> (i, exp)) xs
                               in
                               if List.is_empty already_speced_args then
                                 FuncTable.add funcs
                                   (F_specialized (id, speced))
                                   (str_args, speced_fbody)
                               else
                                 FuncTable.add funcs
                                   (F_specialized
                                      ( id,
                                        List.sort
                                          (fun (i1, _) (i2, _) -> compare i1 i2)
                                          (speced @ already_speced_args) ))
                                   (str_args, speced_fbody)))
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id id, Range ({ pos_fname = path; _ }, _)),
                          Pat_aux (Pat_exp (parg, body), _) ),
                      _ )
                  when String.starts_with path ~prefix:"./sail-riscv" ->
                    let args = pat_to_lst parg |> pats_to_strs in
                    FuncTable.add funcs (F_usual id) (args, body)
                | _ -> ())
              funcls
        | _ -> ())
      ast.defs
  in
  let end_time = Unix.gettimeofday () in
  printfn "func collect %.6f" (end_time -. start_time);

  (* FuncTable.iter
     (fun k v ->
       match k with
       | Spec.F_specialized (id, speced) ->
           let args, body = v in
           if is_name_for_tracing id then
             printfn "speced %s: (%s) ||| %s %s" id (String.concat " " args)
               (String.concat " "
                  (List.map
                     (fun (a, b) -> string_of_int a ^ " : " ^ string_of_exp b)
                     speced))
               (string_of_exp body)
       | F_usual id -> if is_name_for_tracing id then printfn "usual %s" id)
     funcs; *)
  let start_time = Unix.gettimeofday () in
  let g = Call_graph.generate funcs ast in
  let end_time = Unix.gettimeofday () in

  printfn "graph generate %.6f" (end_time -. start_time);

  let () = Call_graph.dump g config.dot_file in

  (* let g = Call_graph.generate funcs in *)

  (* let () =
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
     in *)
  (* Call_graph.dump g config.dot_file; *)
  ()

(* let outs =
     Call_graph.propogate_operands ~g ~aliases
       (List.to_seq [ ("wX", [ "r" ]); ("wF", [ "r" ]); ("wV", [ "r" ]) ])
   in
   let ins =
     Call_graph.propogate_operands ~g ~aliases
       (List.to_seq [ ("rX", [ "r" ]); ("rF", [ "r" ]); ("rV", [ "r" ]) ])
   in

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

       let lst_str ppf out =
         Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
           (fun ppf -> Format.fprintf ppf "%S")
           ppf out
       in

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
              printf
                "@[Hashtbl.add ans \"%s\" { mnemonic=\"%s\"; operands=[%a]; \
                 ins=[%a]; outs=[%a] };@]@,"
                mnemonic mnemonic lst_str opers lst_str ins lst_str outs);

       printf "@[ans@]@ ";
       printf "@]@ ";
       Format.pp_print_cut ppf ();
       printf "@[let find = Hashtbl.find %s @]@," config.ocaml_ident;
       printf "@[let find_opt = Hashtbl.find_opt %s @]@," config.ocaml_ident;
       printf "@[let mem = Hashtbl.mem %s @]@," config.ocaml_ident) *)

let () =
  Arg.parse
    ([
       ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
       ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
     ]
    @ !Mysail.options)
    (fun s -> Mysail.opt_file_arguments := !Mysail.opt_file_arguments @ [ s ])
    "";
  let ast, env, effects = Mysail.process () in
  dump_execute ast env effects
