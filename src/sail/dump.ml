open Checker_core
open Utils
open Analysis_tools

type cfg = {
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config = { ocaml_code = ""; ocaml_ident = ""; dot_file = "" }

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
          | E_let (LB_aux (LB_val (P_aux (pat, _), E_aux (bind, _)), _), _) -> (
              match pat with
              | P_id (Id_aux (Id alias, _))
              | P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)) ->
                  let rec f = function
                    | E_app
                        ( Id_aux (Id ("creg2reg_idx" | "signed" | "unsigned"), _),
                          [ E_aux (E_id (Id_aux (Id arg, _)), _) ] )
                    | E_app
                        ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                          [ _; E_aux (E_id (Id_aux (Id arg, _)), _) ] ) ->
                        add_alias alias arg
                    | E_typ (_, E_aux (e, _)) -> f e
                    | _ -> ()
                  in
                  f bind
              | _ -> ())
          | _ -> ());
          default_iterator.exp_aux self e);
    }
  in
  it.exp it body;
  !aliases

open Libsail
open Spec
open Ast_util
open Instruction

(* rewrites calls extensionEnabled and validDoubleRegs to true and
   in32BitMode depending on the current arch target *)
let rewrite_for_analyze_encdecs ast =
  let open Libsail.Rewriter in
  let rw_funcall e annot =
    let mk_bool_lit lit =
      E_aux
        ( E_lit (mk_lit lit),
          (Unknown, Type_check.(mk_tannot (env_of_annot annot) bool_typ)) )
    in
    match e with
    | E_app (Id_aux (Id id, _), _)
      when id = "extensionEnabled" || id = "validDoubleRegs" ->
        mk_bool_lit L_true
    | E_app (Id_aux (Id id, _), _) when id = "in32BitMode" -> (
        match Utils.target_arch with
        | RV32 -> mk_bool_lit L_true
        | RV64 -> mk_bool_lit L_false
        | _ -> assert false)
    | e -> E_aux (e, annot)
  in
  let rewriters =
    {
      rewriters_base with
      rewrite_exp =
        (fun _ ->
          fold_exp
            {
              id_exp_alg with
              e_aux = (fun (e_aux, annot) -> rw_funcall e_aux annot);
            });
      rewrite_def =
        (let rewrite_mpexp rewriters (MPat_aux (aux, (l, annot))) =
           let aux =
             match aux with
             | MPat_pat mpat -> MPat_pat mpat
             | MPat_when (mpat, exp) ->
                 MPat_when (mpat, rewriters.rewrite_exp rewriters exp)
           in
           MPat_aux (aux, (l, annot))
         in
         let rewrite_mapcl rewriters (MCL_aux (aux, def_annot)) =
           let aux =
             match aux with
             | MCL_bidir (mpexp1, mpexp2) ->
                 MCL_bidir
                   ( rewrite_mpexp rewriters mpexp1,
                     rewrite_mpexp rewriters mpexp2 )
             | MCL_forwards (mpexp, exp) ->
                 MCL_forwards
                   ( rewrite_mpexp rewriters mpexp,
                     rewriters.rewrite_exp rewriters exp )
             | MCL_backwards (mpexp, exp) ->
                 MCL_backwards
                   ( rewrite_mpexp rewriters mpexp,
                     rewriters.rewrite_exp rewriters exp )
           in
           MCL_aux (aux, def_annot)
         in
         let rewrite_mapdef rewriters
             (MD_aux (MD_mapping (id, tannot_opt, mapcls), annot)) =
           MD_aux
             ( MD_mapping
                 (id, tannot_opt, List.map (rewrite_mapcl rewriters) mapcls),
               annot )
         in
         fun self (DEF_aux (aux, def_annot) as def) ->
           match aux with
           | DEF_mapdef mapdef ->
               DEF_aux (DEF_mapdef (rewrite_mapdef self mapdef), def_annot)
           | _ -> rewrite_def self def);
    }
  in
  rewrite_ast_base rewriters ast

let remove_unused_let_binds body =
  let rec remove_unused_let_binds' body =
    let pat_ids = ref [] in
    let collect_let_pats =
      {
        default_iterator with
        exp =
          (fun self (E_aux (e, _) as exp) ->
            let () =
              match e with
              | E_let
                  ( LB_aux
                      (LB_val (P_aux (P_id (Id_aux (Id pat_id, _)), _), _), _),
                    tail_exp )
              | E_let
                  ( LB_aux
                      ( LB_val
                          ( P_aux
                              ( P_typ
                                  (_, P_aux (P_id (Id_aux (Id pat_id, _)), _)),
                                _ ),
                            _ ),
                        _ ),
                    tail_exp ) ->
                  pat_ids := (pat_id, tail_exp) :: !pat_ids
              | _ -> ()
            in
            default_iterator.exp self exp);
      }
    in
    collect_let_pats.exp collect_let_pats body;

    let used = ref [] in
    let collect_uses pat_id =
      {
        default_iterator with
        exp =
          (fun self (E_aux (e, _) as exp) ->
            let () =
              match e with
              | E_id (Id_aux (Id id, _)) when pat_id = id ->
                  used := pat_id :: !used
              | _ -> ()
            in
            default_iterator.exp self exp);
      }
    in
    List.iter
      (fun (pat_id, e) ->
        let check = collect_uses pat_id in
        check.exp check e)
      !pat_ids;

    let cnt_rw = ref 0 in
    let open Rewriter in
    let rw_e e_aux annot =
      match e_aux with
      | E_let
          (LB_aux (LB_val (P_aux (P_id (Id_aux (Id pat_id, _)), _), _), _), tail)
      | E_let
          ( LB_aux
              ( LB_val
                  ( P_aux (P_typ (_, P_aux (P_id (Id_aux (Id pat_id, _)), _)), _),
                    _ ),
                _ ),
            tail )
        when not (List.mem pat_id !used) ->
          incr cnt_rw;
          tail
      | _ -> E_aux (e_aux, annot)
    in
    let rewrites =
      {
        rewriters_base with
        rewrite_exp =
          (fun _ ->
            fold_exp
              {
                id_exp_alg with
                e_aux = (fun (e_aux, annot) -> rw_e e_aux annot);
              });
      }
    in
    let new_body =
      let (E_aux (aux, annot)) = body in
      rewrite_exp rewrites (rw_e aux annot)
    in

    if !cnt_rw <> 0 then remove_unused_let_binds' new_body else new_body
  in
  remove_unused_let_binds' body

let anf_rewrite ast =
  let open Libsail.Rewriter in
  let rw_funcl (FCL_aux (FCL_funcl (id, pexp), (l, annot))) =
    let pexp =
      match pexp with
      | Pat_aux (Pat_exp (pat, body), t) -> (
          let anf = Myanf.anf body in
          try
            let recheked_body =
              Type_check.(check_exp (env_of body) anf (typ_of body))
            in

            (* printfn "\n\n\n\nsuccess %s %s\n%s\n\n" (string_of_id id)
               (string_of_pat pat) (string_of_exp anf); *)
            Pat_aux (Pat_exp (pat, recheked_body), t)
          with exn ->
            printfn "\n\n\n\ncannotqwe %s %s\n%s\n\n" (string_of_id id)
              (string_of_pat pat) (string_of_exp anf);

            (match exn with
            | Type_check.Type_error (_, _, err) ->
                printfn "Type_error %s" (Type_error.string_of_type_error err)
            | Reporting.Fatal_error err ->
                printfn "Type_error";
                Reporting.print_error err
            | _ -> printfn "trash %s" (Printexc.to_string exn));
            pexp)
      | _ -> pexp
    in
    FCL_aux (FCL_funcl (id, pexp), (l, annot))
  in
  let rewrites =
    {
      rewriters_base with
      rewrite_fun =
        (fun _ (FD_aux (FD_function (r, t, funcls), annot)) ->
          FD_aux (FD_function (r, t, List.map rw_funcl funcls), annot));
    }
  in
  rewrite_ast_base rewrites ast

(* funcs that depend on xlen *)
let depend_xlen_funcs = FuncTable.create 1000

let dump_execute ast env effect_info =
  let ast = anf_rewrite ast in
  let ast = rewrite_for_analyze_encdecs ast in
  let assemblies_info = Assembly.get_info ast in

  let t = profile_start () in
  let ast_realize_maps, effect_info, _ =
    (* to be able to evaluate mapping calls  *)
    Mysail.apply_rewrites ast effect_info env [ ("realize_mappings", []) ]
  in
  profile_end "applying rewrites" t;

  let myconst_prop_all_pure =
    Myconstant_propagation.const_prop "" ast_realize_maps (fun _ ->
        (* all funcs pure *) true)
  in

  let myconst_prop =
    Myconstant_propagation.const_prop "" ast_realize_maps (fun id ->
        Effects.function_is_pure id effect_info)
  in

  (* to eval sizeof(xlen) == 32 || == 64 *)
  let empty_const_prop func body =
    let exception FoundXlen in
    let it =
      {
        default_iterator with
        exp_aux =
          (fun self e ->
            match e with
            | E_lit (L_aux (L_num n, Unknown))
              when Nat_big_num.(equal (of_int 32) n || equal (of_int 64) n) ->
                raise FoundXlen
            | _ -> default_iterator.exp_aux self e);
      }
    in
    try
      it.exp it body;
      body
    with FoundXlen ->
      let ref_vars = Constant_propagation.referenced_vars body in
      fst
      @@ myconst_prop
           (fun () -> FuncTable.add depend_xlen_funcs func ())
           ref_vars
           (Bindings.empty, KBindings.empty)
           Bindings.empty body
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
                    let add_usual () =
                      let func = Func.F_usual id in
                      let fbody = remove_unused_let_binds fbody in
                      FuncTable.add funcs func (str_args, fbody)
                    in
                    match
                      (Spec.get_speced_args pargsi, Spec.get_args_to_spec pargsi)
                    with
                    | [], [] ->
                        let func = Func.F_usual id in
                        let fbody = empty_const_prop func fbody in
                        let fbody = remove_unused_let_binds fbody in
                        FuncTable.add funcs func (str_args, fbody)
                    | speced_args, [] ->
                        add_usual ();
                        let func = Func.F_specialized (id, speced_args) in
                        let fbody = empty_const_prop func fbody in
                        let fbody = remove_unused_let_binds fbody in
                        FuncTable.add funcs func (str_args, fbody)
                    | already_speced_args, args_to_spec ->
                        add_usual ();
                        let ref_vars =
                          Constant_propagation.referenced_vars fbody
                        in
                        Spec.product args_to_spec
                        |> List.iter (fun xs ->
                               let func =
                                 let speced =
                                   List.map (fun (i, _, exp) -> (i, exp)) xs
                                 in
                                 Func.F_specialized
                                   ( id,
                                     List.merge
                                       (fun (i1, _) (i2, _) -> compare i1 i2)
                                       speced already_speced_args )
                               in
                               let speced_fbody, _ =
                                 let substs =
                                   Bindings.of_list
                                     (List.map
                                        (fun (_, id, exp) -> (id, exp))
                                        xs)
                                 in
                                 myconst_prop
                                   (fun () ->
                                     FuncTable.add depend_xlen_funcs func ())
                                   ref_vars (substs, KBindings.empty)
                                   Bindings.empty fbody
                               in
                               let speced_fbody =
                                 remove_unused_let_binds speced_fbody
                               in
                               FuncTable.add funcs func (str_args, speced_fbody))
                    )
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id id, Range _),
                          Pat_aux (Pat_exp (parg, body), _) ),
                      _ )
                  when id <> "internal_error" ->
                    let args = pat_to_lst parg |> pats_to_strs in
                    let func = Func.F_usual id in
                    FuncTable.add funcs func (args, empty_const_prop func body)
                | _ -> ())
              funcls
        | _ -> ())
      ast_realize_maps.defs
  in
  profile_end "collect functions" t;

  let data_graphs = FuncTable.create (FuncTable.length funcs) in
  FuncTable.iter
    (fun f (_, body) ->
      printfn "\n\nqweqwe %s\n%s\n" (Func.to_string f) (string_of_exp body);
      FuncTable.add data_graphs f (Data_graph.generate body);
      printfn "")
    funcs;

  let t = profile_start () in
  let g =
    Call_graph.generate funcs
      (fun f vars substs assignes exp ->
        let speced_body, bs = myconst_prop f vars substs assignes exp in
        (remove_unused_let_binds speced_body, bs))
      (fun id -> FuncTable.add depend_xlen_funcs id ())
  in
  profile_end "call graph generation" t;

  FuncTable.iter
    (fun f _ ->
      let reaches =
        Call_graph.get_reachables_from_func g ~start_f:f ~break_ids:[]
      in
      FuncTable.iter (FuncTable.add depend_xlen_funcs) reaches)
    (FuncTable.copy depend_xlen_funcs);

  let outs_csr, ins_csr = Call_graph.analyze_csr g funcs in

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

  FuncTable.iter
    (fun f xs ->
      printfn "%s ouuuuts: %s" (Func.to_string f) (String.concat " " xs))
    outs;

  FuncTable.iter
    (fun f xs ->
      printfn "%s iiiiins: %s" (Func.to_string f) (String.concat " " xs))
    ins;

  let break_ids = [ "translateAddr" ] in
  let mayLoads =
    Call_graph.get_reachables_from_func_id g funcs ~start_id:"read_ram"
      ~break_ids
  in
  let mayStores =
    Call_graph.get_reachables_from_func_id g funcs ~start_id:"write_ram"
      ~break_ids
  in

  let get_arch_from_encdec =
    let encdecs =
      let f id =
        let xs =
          ast.defs
          |> List.find_map (function
               | DEF_aux
                   ( DEF_mapdef
                       (MD_aux (MD_mapping (Id_aux (Id id', _), _, encdecs), _)),
                     _ )
                 when id = id' ->
                   Some encdecs
               | _ -> None)
        in
        match xs with Some xs -> xs | None -> []
      in
      f "encdec" @ f "encdec_compressed"
    in
    let encdec_conds = FuncTable.create 1500 in
    List.iter
      (let process ident args e =
         let args = List.mapi (fun i a -> (i, Ast_util.pat_of_mpat a)) args in
         let empty_const_prop e =
           fst
           @@ myconst_prop_all_pure
                (fun () -> ())
                IdSet.empty
                (Bindings.empty, KBindings.empty)
                Bindings.empty e
         in
         match (get_speced_args args, get_args_to_spec args) with
         | [], [] ->
             FuncTable.add encdec_conds (Func.F_usual ident)
               (empty_const_prop e)
         | speced, [] ->
             FuncTable.add encdec_conds
               (F_specialized (ident, speced))
               (empty_const_prop e)
         | already_speced, to_spec ->
             let ref_vars = Constant_propagation.referenced_vars e in
             product to_spec
             |> List.iter (fun xs ->
                    let func =
                      let speced = List.map (fun (i, _, exp) -> (i, exp)) xs in
                      Func.F_specialized
                        ( ident,
                          List.merge
                            (fun (i1, _) (i2, _) -> compare i1 i2)
                            speced already_speced )
                    in
                    let e, _ =
                      let substs =
                        Bindings.of_list
                          (List.map (fun (_, id, exp) -> (id, exp)) xs)
                      in
                      myconst_prop_all_pure
                        (fun () -> ())
                        ref_vars (substs, KBindings.empty) Bindings.empty e
                    in
                    FuncTable.add encdec_conds func e)
       in
       function
       | MCL_aux
           ( MCL_bidir
               ( MPat_aux
                   ( MPat_when
                       (MP_aux (MP_app (Id_aux (Id ident, _), args), _), e),
                     _ ),
                 _ ),
             _ ) ->
           process ident args e
       | MCL_aux
           ( MCL_bidir
               ( MPat_aux
                   ( MPat_pat (MP_aux (MP_app (Id_aux (Id ident, _), args), _)),
                     _ ),
                 MPat_aux (MPat_when (MP_aux _, e), _) ),
             _ ) ->
           process ident args e
       | _ -> ())
      encdecs;

    fun func ->
      let conds = FuncTable.find_all encdec_conds func in
      if List.is_empty conds then Arch.RV32_RV64
      else
        let archs =
          List.map
            (function
              | E_aux (E_lit (L_aux (L_true, _)), _) -> Utils.target_arch
              | E_aux (E_lit (L_aux (L_false, _)), _) -> (
                  match Utils.target_arch with
                  | RV32 -> RV64
                  | RV64 -> RV32
                  | _ -> assert false)
              | _ -> RV32_RV64)
            conds
        in
        let mem_rv32 = List.mem Arch.RV32 archs in
        let mem_rv64 = List.mem Arch.RV64 archs in
        if mem_rv32 && mem_rv64 then RV32_RV64
        else if mem_rv32 then RV32
        else if mem_rv64 then RV64
        else RV32_RV64
  in

  let check_regs f regs =
    let remove_duplicates ~equal lst =
      List.fold_left
        (fun acc x -> if List.exists (equal x) acc then acc else x :: acc)
        [] lst
    in
    printfn "proccessss %s" (Func.to_string f);
    match FuncTable.find_opt data_graphs f with
    | Some data_g ->
        regs
        |> List.concat_map (fun r ->
               match Data_graph.find_sources data_g r with
               | [] -> assert false
               | [ src ] ->
                   printfn "regqwe %s reg - %s src - %s" (Func.to_string f) r
                     src;
                   if r <> src then [ Operand.GPRPair src ] else [ GPR r ]
               | srcs -> List.map (fun s -> Operand.GPRPair s) srcs)
        |> remove_duplicates ~equal:(fun a b -> Operand.get a = Operand.get b)
    | None -> List.map (fun s -> Operand.GPR s) regs
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
      printf "@[let ans = InstrTable.create 1000 in@]@ ";

      assemblies_info
      |> FuncTable.iter (fun func (mnemonics, operands, imms) ->
             let ins =
               let imms =
                 if
                   List.exists
                     (fun s ->
                       List.mem s (* typo in sail *)
                         [ "vslideup.vi"; "vslidedown.vi"; "vrgather.vi" ])
                     mnemonics
                 then "simm" :: imms
                 else imms
               in
               let regs_ins =
                 match FuncTable.find_opt ins func with
                 | Some ins -> check_regs func ins
                 | None -> []
               in
               regs_ins @ List.map (fun i -> Operand.Imm i) imms
             in
             let outs =
               match FuncTable.find_opt outs func with
               | Some outs -> check_regs func outs
               | None -> []
             in
             let mayStore, mayLoad =
               (FuncTable.mem mayStores func, FuncTable.mem mayLoads func)
             in
             let ins_csr =
               match FuncTable.find_opt ins_csr func with
               | Some ins -> List.map string_of_id ins
               | None -> []
             in
             let outs_csr =
               match FuncTable.find_opt outs_csr func with
               | Some outs -> List.map string_of_id outs
               | None -> []
             in
             let arch = get_arch_from_encdec func in
             if Arch.equal arch Utils.target_arch then
               List.iter
                 (fun mnemonic ->
                   let arch =
                     (* if body of execute depends on a xlen  *)
                     if FuncTable.mem depend_xlen_funcs func then
                       Utils.target_arch
                     else arch
                   in
                   printf_add_instr ppf
                     ({
                        mnemonic;
                        arch;
                        operands;
                        ins;
                        outs;
                        mayLoad;
                        mayStore;
                        ins_csr;
                        outs_csr;
                      }
                       : Instruction.t))
                 mnemonics);

      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find = InstrTable.find %s @]@," config.ocaml_ident;
      printf "@[let find_opt = InstrTable.find_opt %s @]@," config.ocaml_ident;
      printf "@[let mem = InstrTable.mem %s @]@," config.ocaml_ident)

let () =
  (* Libsail.Type_check.set_tc_debug 2; *)
  let sail_args = ref [] in
  Arg.parse
    [
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
      ("-dot", Arg.String (fun s -> config.dot_file <- s), "");
      ("--", Arg.Rest_all (fun xs -> sail_args := xs), "");
    ]
    (fun s -> failwithf "Bad argument: %s\n" s)
    "";
  let ast, env, effect_info =
    Mysail.main (Array.of_list ("sail" :: !sail_args))
  in
  dump_execute ast env effect_info
