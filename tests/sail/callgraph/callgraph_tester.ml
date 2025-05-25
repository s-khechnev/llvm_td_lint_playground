open Analysis_tools
open Libsail
open Ast
open Ast_util
open Spec

let get_speced_get_to_spec_test (ast : 'a Ast_defs.ast) e_info =
  let myconst_prop =
    Myconstant_propagation.const_prop "" ast (fun id ->
        Effects.function_is_pure id e_info)
  in
  let funcs = FuncTable.create 2500 in
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
                      (Id_aux (Id id, _), Pat_aux (Pat_exp (parg, body), _)),
                    _ ) ->
                  let args = pat_to_lst parg |> pats_to_strs in
                  let func = Func.F_usual id in
                  FuncTable.add funcs func (args, body)
              | _ -> ())
            funcls
      | _ -> ())
    ast.defs;
  let g = Call_graph.generate funcs myconst_prop (fun _ -> ()) in
  Call_graph.dump g "graph.dot"

let () =
  Reporting.opt_warnings := false;
  let ast, _, e_info = Mysail.main Sys.argv in
  get_speced_get_to_spec_test ast e_info
