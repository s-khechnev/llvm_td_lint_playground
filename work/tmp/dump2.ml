[@@@ocaml.warnerror "-unused-extension"]

let failwithf fmt = Format.kasprintf failwith fmt
let sprintf fmt = Printf.sprintf fmt

let log fmt =
  if true then Format.kasprintf print_endline fmt
  else Format.ikfprintf (fun _ -> ()) Format.std_formatter fmt

let loge fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt

type cfg = {
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config = { ocaml_code = ""; ocaml_ident = ""; dot_file = "graph.dot" }
let printfn ppf = Format.kasprintf print_endline ppf

include struct
  open Myast_iterator
  open Myast

  exception Match_op_found of Libsail.Type_check.tannot pexp_funcl list

  let has_match =
    {
      default_iterator with
      exp_aux =
        (fun self e ->
          match e with
          | E_match (E_aux (E_id (Id_aux (Id "op", _)), _), xs) ->
              raise (Match_op_found xs)
          | _ -> default_iterator.exp_aux self e);
    }

  (** Checks that we match argument [op] in body *)
  let has_right_match expr =
    try
      has_match.exp has_match expr;
      None
    with Match_op_found xs -> Some xs
end

let is_name_for_tracing = function
  (* | "RISCV_CZERO_EQZ" | "F_UN_TYPE_D" *)
  (* | "RTYPE" | "RISCV_ADD" | "C_ADD"  *)
  (* | "ZICOND_RTYPE" -> *)
  (* | "C_ADDI" -> true *)
  | _ -> false

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
    type t = string option list

    let compare = Stdlib.compare
    let equal = Stdlib.( = )
    let hash = Stdlib.Hashtbl.hash
    let default = []
    let make = Fun.id
  end

  module G = Graph.Imperative.Digraph.ConcreteLabeled (V) (Edge)
  module Top = Graph.Topological.Make (G)

  let g = G.create ()

  let formal_params_hash : (string, formal_params) Hashtbl.t =
    Hashtbl.create 111

  let make_iterator curV is_right_opnd out_register_assmt in_register_assmt
      add_alias try_get_alias =
    {
      default_iterator with
      exp_aux =
        (fun self e ->
          (match e with
          | E_app
              ( Id_aux
                  ( Id
                      ( "wX_bits" | "wF_bits" | "wF_or_X_D" | "wF_or_X_H"
                      | "wF_or_X_S" | "wF_S" | "wF_D" | "wF_H" ),
                    _ ),
                [ E_aux (E_id (Id_aux (Id rd, _)), _); _ ] )
            when is_right_opnd rd ->
              out_register_assmt rd
          | E_app
              ( Id_aux (Id "write_vreg", _),
                [ _; _; _; E_aux (E_id (Id_aux (Id rd, _)), _); _ ] )
            when is_right_opnd rd ->
              out_register_assmt rd
          | E_app
              ( Id_aux (Id "write_vmask", _),
                [ _; E_aux (E_id (Id_aux (Id rd, _)), _); _ ] )
            when is_right_opnd rd ->
              out_register_assmt rd
          | E_app
              ( Id_aux (Id "write_single_element", _),
                [ _; _; E_aux (E_id (Id_aux (Id rd, _)), _); _ ] ) ->
              out_register_assmt rd
          | E_app
              ( Id_aux (Id "process_vlxseg", _),
                [
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rd, _)), _);
                  _;
                  _;
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  E_aux (E_id (Id_aux (Id rs2, _)), _);
                  _;
                  _;
                ] ) ->
              out_register_assmt rd;
              in_register_assmt rs1;
              in_register_assmt rs2
          | E_app
              ( Id_aux (Id "process_vlsseg", _),
                [
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rd, _)), _);
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  E_aux (E_id (Id_aux (Id rs2, _)), _);
                  _;
                  _;
                ] ) ->
              out_register_assmt rd;
              in_register_assmt rs1;
              in_register_assmt rs2
          | E_app
              ( Id_aux (Id "process_vlsegff", _),
                [
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rd, _)), _);
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  _;
                  _;
                ] ) ->
              out_register_assmt rd;
              in_register_assmt rs1
          | E_app
              ( Id_aux (Id "process_vlre", _),
                [
                  _;
                  E_aux (E_id (Id_aux (Id rd, _)), _);
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  _;
                ] ) ->
              out_register_assmt rd;
              in_register_assmt rs1
          | E_app
              ( Id_aux (Id "process_vlseg", _),
                [
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rd, _)), _);
                  _;
                  E_aux (E_id (Id_aux (Id rs, _)), _);
                  _;
                  _;
                ] ) ->
              out_register_assmt rd;
              in_register_assmt rs
          | E_app
              ( Id_aux (Id ("process_rfvv_widen" | "process_rfvv_single"), _),
                [
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  E_aux (E_id (Id_aux (Id rs2, _)), _);
                  E_aux (E_id (Id_aux (Id rd, _)), _);
                  _;
                  _;
                  _;
                ] ) ->
              out_register_assmt rd;
              in_register_assmt rs1;
              in_register_assmt rs2
          | E_app
              ( Id_aux (Id "process_vsseg", _),
                [
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  _;
                  E_aux (E_id (Id_aux (Id rs2, _)), _);
                  _;
                  _;
                ] ) ->
              in_register_assmt rs1;
              in_register_assmt rs2
          | E_app
              ( Id_aux (Id "process_vsxseg", _),
                [
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  E_aux (E_id (Id_aux (Id rs2, _)), _);
                  _;
                  _;
                  _;
                  _;
                  E_aux (E_id (Id_aux (Id rs3, _)), _);
                  E_aux (E_id (Id_aux (Id rs4, _)), _);
                  _;
                  _;
                ] ) ->
              in_register_assmt rs1;
              in_register_assmt rs2;
              in_register_assmt rs3;
              in_register_assmt rs4
          | E_app
              ( Id_aux (Id "process_vssseg", _),
                [
                  _;
                  E_aux (E_id (Id_aux (Id rs1, _)), _);
                  E_aux (E_id (Id_aux (Id rs2, _)), _);
                  _;
                  E_aux (E_id (Id_aux (Id rs3, _)), _);
                  E_aux (E_id (Id_aux (Id rs4, _)), _);
                  _;
                  _;
                ] ) ->
              in_register_assmt rs1;
              in_register_assmt rs2;
              in_register_assmt rs3;
              in_register_assmt rs4
          | E_app
              ( Id_aux (Id "read_vmask_carry", _),
                [ _; _; E_aux (E_lit (L_aux (L_bin "00000", _)), _) ] ) ->
              in_register_assmt "v0"
          | E_app
              ( Id_aux (Id "read_vmask", _),
                [ _; _; E_aux (E_id (Id_aux (Id rs1, _)), _) ] ) ->
              in_register_assmt rs1
          | E_app
              ( Id_aux (Id "read_vmask", _),
                [ _; _; E_aux (E_lit (L_aux (L_bin "00000", _)), _) ] ) ->
              in_register_assmt "v0"
          | E_app
              ( Id_aux (Id "read_single_element", _),
                [ _; _; E_aux (E_id (Id_aux (Id rs1, _)), _) ] ) ->
              in_register_assmt rs1
          | E_app
              ( Id_aux
                  ( Id
                      ("process_fload16" | "process_fload32" | "process_fload64"),
                    _ ),
                [ E_aux (E_id (Id_aux (Id rd, _)), _); _; _ ] ) ->
              out_register_assmt rd
          | E_app
              ( Id_aux (Id "execute", _),
                [
                  E_aux
                    ( E_app (Id_aux (Id rtype, _), [ E_aux (E_tuple args, _) ]),
                      _ );
                ] ) as e ->
              (* log "%s %d rtype = %S, argc = %d" __FILE__ __LINE__ rtype
                 (List.length args); *)
              let f a =
                if Char.uppercase_ascii a.[0] = a.[0] then None
                else
                  match try_get_alias a with
                  | Some alias -> Some alias
                  | None -> Some a
              in
              let spec_args =
                List.map
                  (function
                    | E_aux (E_id (Id_aux (Id id, _)), _)
                    | E_aux
                        ( E_app
                            ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                              [ _; E_aux (E_id (Id_aux (Id id, _)), _) ] ),
                          _ ) ->
                        f id
                    | E_aux
                        ( E_app
                            ( Id_aux (Id "creg2reg_idx", _),
                              [ E_aux (E_id (Id_aux (Id id, _)), _) ] ),
                          _ ) ->
                        f id
                    | E_aux
                        ( E_app
                            ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                              _
                              :: [
                                   E_aux
                                     ( E_app
                                         (Id_aux (Id "bitvector_concat", _), xs),
                                       _ );
                                 ] ),
                          _ ) ->
                        let id =
                          Option.get
                          @@ List.find_map
                               (function
                                 | E_aux (E_id (Id_aux (Id id, _)), _) ->
                                     Some id
                                 | _ -> None)
                               xs
                        in
                        f id
                    | _ -> None)
                  args
              in
              let v_dest = rtype in
              G.add_vertex g curV;
              G.add_vertex g v_dest;
              G.add_edge_e g (v_dest, Edge.make spec_args, curV);
              default_iterator.exp_aux self e
          | E_app
              ( Id_aux
                  ( Id
                      ( "rX_bits" | "rF_D" | "rF_or_X_D" | "rF_or_X_S"
                      | "rF_or_X_H" | "rF_S" | "rF_H" | "rF_bits" ),
                    _ ),
                [ E_aux (E_id (Id_aux (Id rs, _)), _) ] ) ->
              in_register_assmt rs
          | E_app
              ( Id_aux (Id "ext_data_get_addr", _),
                [ E_aux (E_id (Id_aux (Id rs, _)), _); _; _; _ ] ) ->
              in_register_assmt rs
          | E_app
              ( Id_aux (Id "read_vreg", _),
                [ _; _; _; E_aux (E_id (Id_aux (Id rs, _)), _) ] ) ->
              in_register_assmt rs
          | E_app
              ( Id_aux (Id ("get_scalar_fp" | "get_scalar"), _),
                [ E_aux (E_id (Id_aux (Id rs, _)), _); _ ] ) ->
              in_register_assmt rs
          | E_let
              ( LB_aux
                  ( LB_val
                      ( P_aux (P_id (Id_aux (Id alias, _)), _),
                        E_aux
                          ( E_app
                              ( Id_aux (_, _),
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
          | _ -> ());
          default_iterator.exp_aux self e);
    }

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

  (* let process_func fid =
       let visited = Hashtbl.create 10 in
       let rec helper fid =
         let it =
           {
             default_iterator with
             exp_aux =
               (fun self e ->
                 (match e with
                 | E_app (Id_aux (Id id, Range ({ pos_fname = path; _ }, _)), _)
                   when String.starts_with ~prefix:"../sail-riscv/" path
                        && (not @@ Hashtbl.mem visited id) ->
                     print_endline id;
                     Hashtbl.add visited id ();

                     G.add_vertex g1 fid;
                     G.add_vertex g1 id;
                     G.add_edge_e g1 (id, Edge.default, fid);

                     helper id
                 | _ -> ());
                 default_iterator.exp_aux self e);
           }
         in
         match Hashtbl.find_opt funcs fid with
         | Some (args, body) -> it.exp it body
         | None -> ()
       in
       helper fid

     let init_graph cur_v body =
       let rec helper ?fid cur_v =
         let visited = Hashtbl.create 10 in
         let it =
           {
             default_iterator with
             exp_aux =
               (fun self e ->
                 (match e with
                 | E_app (Id_aux (Id id, Range ({ pos_fname = path; _ }, _)), args)
                   when String.starts_with ~prefix:"../sail-riscv/" path
                        && (not @@ Hashtbl.mem visited id) -> (
                     Format.printf "qwe %s\n" id;
                     match Hashtbl.find_opt funcs id with
                     | Some (dest_args, _) ->
                         let src_args =
                           List.map
                             (function
                               | E_aux (E_id (Id_aux (Id arg_id, _)), _) -> arg_id
                               | _ -> "trash")
                             args
                         in
                         let args = List.combine src_args dest_args in
                         G.add_vertex g1 cur_v;
                         G.add_vertex g1 id;
                         G.add_edge_e g1 (id, args, cur_v);

                         Hashtbl.add visited id ();

                         helper id ~fid:id
                     | None -> ())
                 | _ -> ());
                 default_iterator.exp_aux self e);
           }
         in
         match fid with
         | Some fid -> (
             match Hashtbl.find_opt funcs fid with
             | Some (_, body) -> it.exp it body
             | None -> ())
         | None -> it.exp it body
       in
       helper cur_v *)

  let saturate_graph is_out extend_out is_in extend_in =
    let on_edge : V.t * Edge.t * V.t -> _ =
     fun (v_from, spec_args, vdest) ->
      let formal_params_dest =
        match Hashtbl.find formal_params_hash v_from with
        | exception Not_found ->
            failwithf "Can't get formal params for %s" vdest
        | xs -> xs
      in
      if is_name_for_tracing v_from || is_name_for_tracing vdest then (
        log "vfrom = %s, dest = %s" v_from vdest;
        log "spec_args = %a" pp_formal_params spec_args;
        log "dest_args = %a" pp_formal_params formal_params_dest);
      assert (List.length spec_args = List.length formal_params_dest);

      let interestring_args =
        List.map2
          (fun a b ->
            match (a, b) with
            | _, None | None, _ -> None
            | Some x, Some y -> Some (x, y))
          spec_args formal_params_dest
        |> List.filter_map Fun.id
      in
      if is_name_for_tracing v_from || is_name_for_tracing vdest then
        log "\nInteresting args for %S: %s" v_from
          (String.concat " "
             (List.map (fun (a, b) -> sprintf "(%s,%s)" a b) interestring_args));
      List.iter
        (fun (spec, dest) ->
          if is_out v_from ~arg:dest then extend_out vdest ~arg:spec;
          if is_in v_from ~arg:dest then extend_in vdest ~arg:spec)
        interestring_args;
      ()
    in
    Top.iter
      (fun v ->
        let edges = G.succ_e g v in
        List.iter on_edge edges)
      g
end

type collected_info =
  | CI_hacky of string * int (* Cname + index of 'op' constructor*)
  | CI_default of string

type implementation_kind =
  | IK_straight of string  (**  [C_MUL(rsdc, rs2c)] *)
  | IK_multidef of string * string list  (** [AMO(op, aq, ...)] *)
  | IK_singledef of string * string
      (** [ZICOND_RTYPE(rs2, rs1, rd, RISCV_CZERO_EQZ)]
          So called hacky definition
       *)

(* let classify_def key args body =
   let open Myast in
   let has_constructor_arg =
     List.find_map
       (fun (a, b) ->
         match Enum_hashtbl.find b with
         | xs -> if List.mem a xs then Some a else None
         | exception Not_found -> None)
       args
   in
   match has_constructor_arg with
   | Some s -> IK_singledef (key, s)
   | None -> (
       match has_right_match body with
       | None -> IK_straight key
       | Some args ->
           IK_multidef
             ( key,
               ListLabels.filter_map args ~f:(function
                 | Pat_aux (Pat_exp (P_aux (P_id (Id_aux (Id name, _)), _), _), _)
                   ->
                     Some name
                 | _ -> None) )) *)

let dump_execute jfile =
  let collected : (string, collected_info) Hashtbl.t = Hashtbl.create 100 in
  let weird_stuff : (string, unit) Hashtbl.t = Hashtbl.create 100 in
  let out_info : (string, string list) Hashtbl.t = Hashtbl.create 100 in
  let in_info : (string, string list) Hashtbl.t = Hashtbl.create 100 in
  let open Myast in
  let on_rtype key pargs body =
    let _ : Libsail.Type_check.tannot exp = body in
    (* let args_w_typ =
         match pargs with
         | [ P_aux (P_tuple ps, _) ] ->
             List.filter_map
               (function
                 | P_aux (P_id (Id_aux (Id id, _)), (_, tannot)) -> (
                     match Obj.magic tannot with
                     | Some { env; typ; _ }, _ -> (
                         match typ with
                         | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                         | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) ->
                             Some (id, typ_id)
                         | _ -> None)
                     | _ -> None)
                 | _ -> None)
               ps
         | [ P_aux (P_id (Id_aux (Id id, _)), _) ] -> [ (id, "qwe") ]
         | _ -> []
       in *)
    let args =
      match pargs with
      | [ P_aux (P_tuple ps, _) ] ->
          List.filter_map
            (function
              | P_aux (P_id (Id_aux (Id id, _)), _) -> Some id | _ -> None)
            ps
      | [ P_aux (P_id (Id_aux (Id id, _)), _) ] -> [ id ]
      | _ -> []
    in

    (* let () =
         match classify_def key args_w_typ body with
         | IK_straight s -> Format.printf "IK_straight %s\n" s
         | IK_singledef (key, op) -> Format.printf "IK_singledef %s - %s\n" key op
         | IK_multidef (key, xs) ->
             (* log "IK_multidef %S: %a" key
                Format.(
                  pp_print_list
                    ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
                    pp_print_string)
                xs *)
             ()
       in *)
    let out_args, in_args =
      let in_args =
        match
          List.find_all
            (fun s ->
              String.starts_with ~prefix:"imm" s
              || String.starts_with ~prefix:"uimm" s
              || String.starts_with ~prefix:"simm" s
              || String.starts_with ~prefix:"bs" s
              || String.starts_with ~prefix:"shamt" s
              || String.starts_with ~prefix:"rnum" s
              || String.starts_with ~prefix:"rm" s
              || String.starts_with ~prefix:"constantidx" s
              || String.starts_with ~prefix:"vm" s
              || String.starts_with ~prefix:"pred" s
              || String.starts_with ~prefix:"succ" s)
            args
        with
        | [] -> ref []
        | xs when not @@ String.equal "VMVRTYPE" key -> ref xs
        | _ -> ref []
      in
      (* let in_args = ref [] in *)
      let out_args = ref [] in
      let aliases = Hashtbl.create 100 in
      let iterator =
        Collect_out_info.make_iterator
          (Collect_out_info.V.make key)
          (fun _ -> true)
          (fun s ->
            if not (List.mem s !out_args) then out_args := s :: !out_args)
          (fun s -> if not (List.mem s !in_args) then in_args := s :: !in_args)
          (fun alias arg ->
            if List.mem arg args then Hashtbl.add aliases alias arg
            else if Hashtbl.mem aliases arg then
              Hashtbl.replace aliases alias arg)
          (Hashtbl.find_opt aliases)
      in
      iterator.exp iterator body;

      if is_name_for_tracing key then (
        log "Out_args for %S: %a" key
          Format.(pp_print_list pp_print_string)
          !out_args;
        Format.printf "count: %d\n" (Hashtbl.length aliases);
        Hashtbl.iter (fun k v -> Format.printf "%s -> %s\n" k v) aliases);

      let f a =
        List.map
          (fun arg ->
            match Hashtbl.find_opt aliases arg with
            | Some alias -> alias
            | None -> arg)
          !a
      in
      (f out_args, f in_args)
    in

    Hashtbl.add in_info key in_args;

    let argidx =
      let formal_params =
        match pargs with
        | [ P_aux (P_lit (L_aux (L_unit, _)), _) ] -> []
        | [ P_aux (P_id (Id_aux (Id id, _)), _) ] -> [ Some id ]
        | [ P_aux (P_tuple ps, _) ] ->
            List.map
              (function
                | P_aux (P_id (Id_aux (Id id, _)), _) -> Some id | _ -> None)
              ps
        | _ ->
            Format.eprintf "%a"
              (Format.pp_print_list (Myast.pp_pat Myast.pp_tannot))
              pargs;
            failwithf "Extraction of formal params not implemented for %s" key
      in
      Hashtbl.add Collect_out_info.formal_params_hash key formal_params;
      let exception Arg_found of int in
      try
        List.iteri
          (fun i x -> if x = Some "op" then raise (Arg_found i))
          formal_params;
        -1
      with Arg_found n -> n
    in

    let () =
      match has_right_match body with
      | None when argidx = -1 ->
          (* No argument called op *)
          (* printfn "%s %d key = %S" __FUNCTION__ __LINE__ key; *)
          Hashtbl.add collected key (CI_default key);
          Hashtbl.add out_info key out_args
      | None ->
          (* printfn "%s %d key = %S" __FUNCTION__ __LINE__ key; *)
          (* failwithf "can't decide what to do when key = %S" key *)
          Hashtbl.add weird_stuff key ()
      | Some args ->
          (* log "The clause for %S HAS right match\n%!" key; *)
          Hashtbl.add out_info key out_args;
          ListLabels.iter args ~f:(function
            | Pat_aux (Pat_exp (P_aux (P_id (Id_aux (Id name, _)), _), _), _) ->
                (* Hashtbl.replace collected name (CI_hacky (key, argidx)); *)
                (* Hashtbl.replace collected key (CI_hacky (name, argidx)); *)
                (* Hashtbl.add out_info name out_args; *)
                (* Hashtbl.add out_info key out_args *)
                if is_name_for_tracing name then
                  log "Adding hacky: key = %s, name = %s" key name;
                Hashtbl.add collected name (CI_hacky (key, argidx));
                Hashtbl.add out_info key out_args;
                Hashtbl.add out_info name out_args;
                Hashtbl.add in_info name in_args
            | _ -> ())
    in
    ()
  in
  let () =
    let json = In_channel.with_open_text jfile Yojson.Safe.from_channel in
    match json with
    | `List xs ->
        List.iter
          (fun j ->
            match Myast.def_of_yojson Myast.tannot_of_yojson j with
            | Result.Error err ->
                Format.eprintf "Error: %s\n%!" err;
                exit 1
            | Ok ast -> (
                match ast with
                | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, body), _)), _)
                  ->
                    ListLabels.iter body ~f:(function
                      | FCL_aux (FCL_funcl (Id_aux (Id "execute", _), fdecl), _)
                        -> (
                          match fdecl with
                          | Pat_aux
                              ( Pat_exp
                                  ( P_aux
                                      ( (P_app (Id_aux (Id name, _), pargs) as
                                         aux),
                                        _ ),
                                    exp ),
                                _ )
                            when not (String.equal name "CSR") (* fix me *) -> (
                              match (name, pargs) with
                              | _ ->
                                  if is_name_for_tracing name then
                                    printfn "@[%s: %a@]@," name
                                      (pp_pat_aux Myast.pp_tannot)
                                      aux;
                                  on_rtype name pargs exp)
                          | _ -> ())
                      | _ -> ())
                | _ -> ()))
          xs
    | _ -> assert false
  in
  let () =
    Collect_out_info.saturate_graph
      (fun op ~arg ->
        match Hashtbl.find out_info op with
        | exception Not_found -> false
        | xs -> List.mem arg xs)
      (fun op ~arg ->
        match Hashtbl.find out_info op with
        | exception Not_found -> Hashtbl.add out_info op [ arg ]
        | xs when not (List.mem arg xs) ->
            Hashtbl.replace out_info op (arg :: xs)
        | _ -> ())
      (fun op ~arg ->
        match Hashtbl.find in_info op with
        | exception Not_found -> false
        | xs -> List.mem arg xs)
      (fun op ~arg ->
        match Hashtbl.find in_info op with
        | exception Not_found -> Hashtbl.add in_info op [ arg ]
        | xs when not (List.mem arg xs) -> Hashtbl.replace in_info op (arg :: xs)
        | _ -> ())
  in
  Collect_out_info.dump_graph ();
  assert (config.ocaml_ident <> "");
  assert (config.ocaml_code <> "");
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[include From6159_helper@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hash_info.create 1000 in@]@ ";

      collected
      |> Hashtbl.iter (fun key ->
             let out =
               match Hashtbl.find out_info key with
               | xs -> xs
               | exception Not_found -> []
             in
             let inputs =
               match Hashtbl.find in_info key with
               | xs -> xs
               | exception Not_found -> []
             in
             let out_str ppf out =
               Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
                 (fun ppf -> Format.fprintf ppf "%S")
                 ppf out
             in
             function
             | CI_default name ->
                 printf "@[def ans  %S {out=[%a]; inputs=[%a]};@]@," name
                   out_str out out_str inputs
             | CI_hacky (name, n) ->
                 printf "@[hacky ans %S %S %d {out=[%a]; inputs=[%a]};@]@," name
                   key n out_str out out_str inputs);
      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let lookup_exn = Hash_info.find %s @]@," config.ocaml_ident;
      printf "@[let mem = Hash_info.mem %s @]@," config.ocaml_ident;

      printf "let %s_hacky = [ " config.ocaml_ident;
      weird_stuff |> Hashtbl.iter (fun s () -> printf "%S; " s);
      printf "]\n\n%!")

let () =
  Arg.parse
    [
      ("-dump-execute", Arg.String (fun s -> dump_execute s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun s -> failwithf "Bad argument: %S" s)
    ""
