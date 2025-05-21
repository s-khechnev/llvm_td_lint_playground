open Myast
module G = Graph.Imperative.Digraph.Concrete (String)
open Libsail
open Ast_util
open Type_check
open Myast_iterator

let generate exp =
  let g = G.create () in
  let collect_ids_it src_id =
    {
      default_iterator with
      exp =
        (fun self (E_aux (e, _) as exp) ->
          let () =
            match e with
            | E_id dst_id
              when match Env.lookup_id dst_id (env_of exp) with
                   | Local _ | Register _ -> true
                   | _ -> false ->
                let dst_id = string_of_id dst_id in
                if dst_id <> src_id then G.add_edge g src_id dst_id
            | _ -> ()
          in
          default_iterator.exp_aux self e);
    }
  in
  let collect_let_binds_it =
    {
      default_iterator with
      let_bind =
        (fun self (LB_aux (LB_val (pat, e), _)) ->
          let () =
            match e with
            | E_aux (E_id _, _) -> ()
            | _ ->
                IdSet.iter
                  (fun id ->
                    let collect_ids_it = collect_ids_it (string_of_id id) in
                    collect_ids_it.exp collect_ids_it e)
                  (pat_ids pat)
          in
          default_iterator.exp self e);
    }
  in
  collect_let_binds_it.exp collect_let_binds_it exp;
  g

let rec dfs g ~start_v ~on_edge =
  G.iter_succ_e
    (fun e ->
      on_edge e;
      dfs g
        ~start_v:
          (let _, v_dst = e in
           v_dst)
        ~on_edge)
    g start_v

let find_sources g id =
  if (not (G.mem_vertex g id)) || List.is_empty (G.succ g id) then [ id ]
  else
    let result = ref [] in
    let on_edge (_, dst) =
      if List.is_empty (G.succ g dst) then result := dst :: !result
    in
    dfs g ~start_v:id ~on_edge;
    !result
