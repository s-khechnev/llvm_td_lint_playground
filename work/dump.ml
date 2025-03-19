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

  type formal_params = string option list

  let pp_formal_params ppf : formal_params -> unit =
    let open Format in
    fprintf ppf "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf "; ")
         (fun ppf -> function
           | None -> fprintf ppf "_"
           | Some x -> pp_print_string ppf x))

  module V = struct
    type t = string

    let make l = l
    let compare = Stdlib.compare
    let equal = Stdlib.( = )
    let hash = Stdlib.Hashtbl.hash
  end

  module Edge = struct
    type t = (string * string) list

    let compare = Stdlib.compare
    let equal = Stdlib.( = )
    let hash = Stdlib.Hashtbl.hash
    let default = []
    let make = Fun.id
  end

  module G = Graph.Imperative.Digraph.ConcreteLabeled (V) (Edge)
  module Top = Graph.Topological.Make (G)

  let formal_params_hash : (string, formal_params) Hashtbl.t =
    Hashtbl.create 111

  let g = G.create ()
  let aliases : (string, (string * string) list) Hashtbl.t = Hashtbl.create 500

  let rec helper src_id =
    let rec extract_arg = function
      | E_aux (E_id (Id_aux (Id id, _)), _) -> id
      | E_aux (E_lit (L_aux (L_unit, _)), _) -> "()"
      | E_aux (E_lit (L_aux (L_num n, _)), _) -> Z.to_string n
      | E_aux (E_lit (L_aux (L_true, _)), _) -> "L_true"
      | E_aux (E_lit (L_aux (L_false, _)), _) -> "L_false"
      | E_aux (E_lit (L_aux (L_bin bn, _)), _) -> "0b" ^ bn
      | E_aux (E_lit (L_aux (L_string s, _)), _) -> s
      | E_aux (E_app (Id_aux (Id f_id, _), args), _) -> (
          helper f_id;
          match args with
          | [ x ] -> extract_arg x
          (* write_single_element(... vd + to_bits(...), ...) *)
          | [
              E_aux (E_app (Id_aux (Id "to_bits", _), _), _);
              E_aux (E_id (Id_aux (Id id, _)), _);
            ]
          | [
              E_aux (E_id (Id_aux (Id id, _)), _);
              E_aux (E_app (Id_aux (Id "to_bits", _), _), _);
            ]
            when String.equal f_id "add_bits" ->
              id
          | _ -> "noname_!")
      | _ ->
          (* Format.printf "%a\n"
               (Myast.pp_exp Myast.pp_tannot)
               src;
             Format.printf
               "Arg's extraction not implemented for %s %s\n"
               cur_v id; *)
          "get_arg_errFIXX"
    in
    let f src_args dst_id =
      match Hashtbl.find_opt funcs dst_id with
      | Some (dst_args, _) ->
          let args =
            List.map2 (fun dst src -> (dst, extract_arg src)) dst_args src_args
          in

          G.add_vertex g src_id;
          G.add_vertex g dst_id;
          G.add_edge_e g (dst_id, args, src_id);

          helper dst_id
      | None -> ()
    in
    let it =
      {
        default_iterator with
        exp_aux =
          (fun self e ->
            match e with
            | E_app
                ( Id_aux (Id "execute", _),
                  [
                    E_aux
                      ( E_app
                          ( Id_aux (Id dst_id, _),
                            [ E_aux (E_tuple src_args, _) ] ),
                        _ );
                  ] ) ->
                f src_args dst_id
            (* default_iterator.exp_aux self e *)
            | E_app
                ( Id_aux (Id dst_id, Range ({ pos_fname = path; _ }, _)),
                  src_args )
              when String.starts_with path ~prefix:"../sail-riscv/model"
                   (* && (not @@ Hashtbl.mem executes dst_id) *)
                   && (not @@ String.equal dst_id src_id) ->
                f src_args dst_id;
                default_iterator.exp_aux self e
            | _ -> default_iterator.exp_aux self e);
      }
    in
    match Hashtbl.find_opt funcs src_id with
    | Some (_, body) -> it.exp it body
    | None -> ()

  let dump_graph () =
    let module Dot = Graph.Graphviz.Dot (struct
      include G

      let vertex_name = Fun.id
      let get_subgraph _ = None
      let vertex_attributes _ = []
      let graph_attributes _ = []
      let edge_attributes _ = []
      let default_vertex_attributes _ = []
      let default_edge_attributes _ = []
    end) in
    Out_channel.with_open_text config.dot_file (fun ch -> Dot.output_graph ch g)

  let propogate_operands info =
    let result : (string, string list) Hashtbl.t = Hashtbl.create 300 in
    let () = Hashtbl.add_seq result (List.to_seq info) in

    let on_edge : V.t * Edge.t * V.t -> unit =
     fun (v_src, args, v_dst) ->
      match Hashtbl.find_opt result v_src with
      | Some ins -> (
          let mapped =
            List.filter_map
              (fun (src, dst) -> if List.mem src ins then Some dst else None)
              args
          in

          let mapped =
            List.map
              (fun op ->
                (* printfn "mapped: not found alias for %s" v_dst; *)
                match Hashtbl.find_opt aliases v_dst with
                | Some aliases -> (
                    match List.assoc_opt op aliases with
                    | Some alias -> alias
                    | None -> op)
                | None -> op)
              mapped
          in

          let qwe =
            List.filter
              (fun op ->
                not @@ List.exists (fun (a, _) -> String.equal a op) args)
              ins
          in

          let res_ins =
            List.filter_map
              (fun a ->
                if String.equal a "noname_!" then None
                else if String.equal a "0b00000" then Some "v0"
                else Some a)
              (qwe @ mapped)
          in

          let res_ins =
            List.map
              (fun op ->
                (* printfn "v_src: not found alias for %s" v_src; *)
                match Hashtbl.find_opt aliases v_src with
                | Some aliases -> (
                    match List.assoc_opt op aliases with
                    | Some alias -> alias
                    | None -> op)
                | None -> op)
              res_ins
          in

          match Hashtbl.find_opt result v_dst with
          | Some dst_ins ->
              Hashtbl.replace result v_dst
                (List.sort_uniq compare (res_ins @ dst_ins))
          | None -> Hashtbl.add result v_dst res_ins
          (* Format.printf "%s - " v_src;
             List.iter (fun (a, b) -> Format.printf "(%s, %s) " a b) args;
             Format.printf "- %s\n" v_dst;
             Format.printf "%s QWE: %s\n" v_src (String.concat " " qwe);
             Format.printf "%s OPERS: %s\n" v_src (String.concat " " ins);
             Format.printf "mapped: %s\n" (String.concat " " mapped);
             Format.printf "%s OPERS: %s\n\n" v_dst
               (String.concat " " (Hashtbl.find result v_dst)) *))
      | None -> ()
    in

    let rec dfs v =
      G.iter_succ_e
        (fun e ->
          on_edge e;
          dfs
            (let _, _, v_dst = e in
             v_dst))
        g v
    in
    List.iter (fun (v, _) -> if G.mem_vertex g v then dfs v) info;

    result

  (* Format.printf "REPORT\n";
     Hashtbl.iter
       (fun k v -> Format.printf "%s - ins: %s\n" k (String.concat " " v))
       result;
     Format.printf "REPORT\n\n\n" *)
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

  let () =
    Hashtbl.iter
      (fun name (args, body) ->
        Collect_out_info.helper name;
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

  Collect_out_info.dump_graph ();

  let outs =
    Collect_out_info.propogate_operands
      [ ("wX", [ "r" ]); ("wF", [ "r" ]); ("wV", [ "r" ]) ]
  in

  let ins =
    Collect_out_info.propogate_operands
      [ ("rX", [ "r" ]); ("rF", [ "r" ]); ("rV", [ "r" ]) ]
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
