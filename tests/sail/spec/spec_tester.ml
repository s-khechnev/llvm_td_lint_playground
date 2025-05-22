open Analysis_tools
open Libsail
open Ast
open Ast_util

let get_speced_get_to_spec_test (ast : 'a Ast_defs.ast) =
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
  List.iter
    (function
      | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
          List.iter
            (function
              | FCL_aux
                  (FCL_funcl (Id_aux (Id id, _), Pat_aux (Pat_exp (p, _), _)), _)
                ->
                  let pargs = pat_to_lst p |> List.mapi (fun i p -> (i, p)) in
                  let xs = Spec.get_args_to_spec pargs in
                  let s =
                    String.concat " "
                      (List.map
                         (fun (i, id, exps) ->
                           Format.sprintf "(%d, %s, [ %s ])" i (string_of_id id)
                             (String.concat "; " (List.map string_of_exp exps)))
                         xs)
                  in
                  Format.printf "%s args to spec: %s\n" id s;

                  let xs = Spec.get_speced_args pargs in
                  let s =
                    String.concat " "
                      (List.map
                         (fun (i, e) ->
                           Format.sprintf "(%d, %s)" i (string_of_exp e))
                         xs)
                  in
                  Format.printf "%s speced args: %s\n" id s
              | _ -> ())
            funcls
      | _ -> ())
    ast.defs

let () =
  Reporting.opt_warnings := false;
  let ast, _, _ = Mysail.main Sys.argv in
  get_speced_get_to_spec_test ast
