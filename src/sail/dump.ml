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

(* funcs that depend on xlen *)
let depend_xlen_funcs = FuncTable.create 1000

let dump_execute ast env effect_info =
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
                    let () =
                      let func = Func.F_usual id in
                      FuncTable.add funcs func (str_args, fbody)
                    in
                    match
                      (Spec.get_speced_args pargsi, Spec.get_args_to_spec pargsi)
                    with
                    | [], [] ->
                        let func = Func.F_usual id in
                        let fbody = empty_const_prop func fbody in
                        FuncTable.add funcs func (str_args, fbody)
                    | speced_args, [] ->
                        let func = Func.F_specialized (id, speced_args) in
                        let fbody = empty_const_prop func fbody in
                        FuncTable.add funcs func (str_args, fbody)
                    | already_speced_args, args_to_spec ->
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

  let t = profile_start () in
  let g =
    Call_graph.generate funcs myconst_prop (fun id ->
        FuncTable.add depend_xlen_funcs id ())
  in
  profile_end "call graph generation" t;

  FuncTable.iter
    (fun f _ ->
      let reaches =
        Call_graph.get_reachables_from_func g ~start_f:f ~break_ids:[]
      in
      FuncTable.iter (FuncTable.add depend_xlen_funcs) reaches)
    (FuncTable.copy depend_xlen_funcs);

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
             let mayStore, mayLoad =
               (FuncTable.mem mayStores func, FuncTable.mem mayLoads func)
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
                     ({ mnemonic; arch; operands; ins; outs; mayLoad; mayStore }
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
