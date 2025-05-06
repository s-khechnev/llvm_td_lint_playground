open Checker_core
open Utils

type cfg = {
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config = { ocaml_code = ""; ocaml_ident = ""; dot_file = "graph.dot" }
let is_name_for_tracing = function "" -> true | _ -> false

open Myast
open Myast_iterator

let collect_aliases body =
  let aliases = ref [] in
  let add_alias alias id = aliases := (alias, id) :: !aliases in
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
  it.exp it body;
  !aliases

open Libsail

let dump_execute ast env effect_info =
  let open Spec in
  let open Ast_util in
  let assemblies_info = Assembly.get_info ast in

  let t = profile_start () in
  let ast, effect_info, _ =
    (* to be able to evaluate mapping calls  *)
    Mysail.apply_rewrites ast effect_info env [ ("realize_mappings", []) ]
  in
  profile_end "applying rewrites" t;

  let myconst_prop =
    Myconstant_propagation.const_prop "" ast (fun id ->
        Effects.function_is_pure id effect_info)
  in

  let t = profile_start () in
  let funcs = FuncTable.create 2500 in
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
                    let pargs = pat_to_lst parg in
                    let str_args = pats_to_strs pargs in
                    let pargsi = List.mapi (fun i p -> (i, p)) pargs in
                    match
                      (Spec.get_speced_args pargsi, Spec.get_args_to_spec pargsi)
                    with
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
                        Spec.product args_to_spec
                        |> List.iter (fun xs ->
                               let speced_fbody, _ =
                                 let substs =
                                   Bindings.of_list
                                     (List.map
                                        (fun (_, id, exp) -> (id, exp))
                                        xs)
                                 in
                                 myconst_prop ref_vars (substs, KBindings.empty)
                                   Bindings.empty fbody
                               in
                               let speced =
                                 List.map (fun (i, _, exp) -> (i, exp)) xs
                               in
                               FuncTable.add funcs
                                 (F_specialized
                                    ( id,
                                      List.merge
                                        (fun (i1, _) (i2, _) -> compare i1 i2)
                                        speced already_speced_args ))
                                 (str_args, speced_fbody)))
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id id, Range ({ pos_fname = path; _ }, _)),
                          Pat_aux (Pat_exp (parg, body), _) ),
                      _ )
                  when String.starts_with path ~prefix:"../../sail-riscv"
                       && id <> "internal_error" ->
                    let args = pat_to_lst parg |> pats_to_strs in
                    FuncTable.add funcs (F_usual id) (args, body)
                | _ -> ())
              funcls
        | _ -> ())
      ast.defs
  in
  profile_end "collect functions" t;

  let t = profile_start () in
  let g = Call_graph.generate funcs myconst_prop in
  profile_end "call graph generation" t;

  let aliases : (string * string) list FuncTable.t = FuncTable.create 500 in
  let () =
    FuncTable.iter
      (fun f (_, body) ->
        let cur_aliases = collect_aliases body in
        if List.is_empty cur_aliases then ()
        else FuncTable.add aliases f cur_aliases)
      funcs;

    if debug then
      FuncTable.iter
        (fun f aliases ->
          printfn "%s aliases:" (Func.get_id f);
          List.iter (fun (alias, id) -> printfn "%s -> %s" alias id) aliases)
        aliases
  in

  let outs =
    Call_graph.propogate_operands ~g ~aliases funcs
      [ (* funcs for writing to regs *) ("wX", "r"); ("wF", "r"); ("wV", "r") ]
  in
  let ins =
    Call_graph.propogate_operands ~g ~aliases funcs
      [
        (* funcs for reading from regs *) ("rX", "r"); ("rF", "r"); ("rV", "r");
      ]
  in

  Call_graph.dump g config.dot_file;
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[open Checker_core.Instruction@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 1000 in@]@ ";

      assemblies_info
      |> FuncTable.iter (fun func (mnemonics, operands, imms) ->
             let ins =
               let imms =
                 (* typo in sail *)
                 if
                   List.exists
                     (fun s ->
                       List.mem s
                         [ "vslideup.vi"; "vslidedown.vi"; "vrgather.vi" ])
                     mnemonics
                 then "simm" :: imms
                 else imms
               in
               let regs_ins =
                 match FuncTable.find_opt ins func with
                 | Some ins -> ins
                 | None -> []
               in
               regs_ins @ imms
             in
             let outs =
               match FuncTable.find_opt outs func with
               | Some outs -> outs
               | None -> []
             in
             List.iter
               (fun mnemonic ->
                 printf_add_instr ppf
                   ({ mnemonic; operands; ins; outs } : Instruction.t))
               mnemonics);

      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find = Hashtbl.find %s @]@," config.ocaml_ident;
      printf "@[let find_opt = Hashtbl.find_opt %s @]@," config.ocaml_ident;
      printf "@[let mem = Hashtbl.mem %s @]@," config.ocaml_ident)

let () =
  let sail_args = ref [] in
  Arg.parse
    [
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
      ("--", Arg.Rest_all (fun xs -> sail_args := xs), "");
    ]
    (fun s -> failwithf "Bad argument: %s\n" s)
    "";
  let ast, env, effect_info =
    Mysail.main (Array.of_list ("sail" :: !sail_args))
  in
  dump_execute ast env effect_info
