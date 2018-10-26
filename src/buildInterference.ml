open AProgram
open RProgram
open Registers
open Helper
open List

let find_in_map key map : aarg list =
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map : unit =
  let current_value = find_in_map key map in
  Hashtbl.remove map key;
  Hashtbl.add map key (List.sort_uniq compare (value :: current_value))

let add_bidirected_edge n1 n2 map : unit =
  append_to_value n1 n2 map;
  append_to_value n2 n1 map

let rec add_bidirected_edges_if cnd (d: aarg) (targets: aarg list) map =
  match targets with
  | n :: t ->
    if cnd n then (add_bidirected_edge d n map; add_bidirected_edges_if cnd d t map)
    else add_bidirected_edges_if cnd d t map
  | [] -> ()

let rec add_directed_edges nodes targets map =
  match nodes with
  | h :: t ->
    List.iter (fun e -> append_to_value h e map) targets;
    add_directed_edges t targets map
  | [] -> ()

let get_live_vectors live_vars var_types =
  List.filter (fun v ->
    match Hashtbl.find var_types (get_avar_name v) with
    | TypeVector _ -> true
    | _ -> false
  ) live_vars

let rec build_graph stmts live_afters map var_types : interference =
  let live_vars = head (live_afters) in
  match stmts with
  | Movq (s, d) :: t | Movzbq(s, d) :: t ->
    (* add the edge (d, v) for every v of Lafter(k) unless v = d or v = s. *)
    add_bidirected_edges_if (fun v -> v <> d && v <> s) d live_vars map;
    build_graph t (tail live_afters) map var_types
  | Addq (s, d) :: t | Subq (s, d) :: t (* | XOrq :: t ? *)->
    (* add the edge (d, v) for every v of Lafter(k) unless v = d. *)
    add_bidirected_edges_if (fun v -> v <> d) d live_vars map;
    build_graph t (tail live_afters) map var_types
  | IMulq (s, d) :: t ->
    add_bidirected_edges_if (fun v -> v <> d) d live_vars map;
    add_directed_edges live_vars [Reg Rdx] map;
    build_graph t (tail live_afters) map var_types
  | Cqto :: t | IDivq _ :: t ->
    add_directed_edges live_vars [Reg Rdx] map;
    build_graph t (tail live_afters) map var_types
  | Callq _ :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_directed_edges live_vars caller_save_aregisters map;
    build_graph t (tail live_afters) map var_types
  | AIf ((c, s, d), thn_instrs, thn_lafter, els_instrs, els_lafter) :: t ->
    let _ = build_graph thn_instrs thn_lafter map var_types in
    let _ = build_graph els_instrs els_lafter map var_types in
    build_graph t (tail live_afters) map var_types
  | AWhile (cnd_instrs, cnd_lafter, (c, s, d), thn_instrs, thn_lafter) :: t ->
    let _ = build_graph cnd_instrs cnd_lafter map var_types in
    let _ = build_graph thn_instrs thn_lafter map var_types in
    build_graph t (tail live_afters) map var_types

  (* if a vector-typed variable is live during a call to the collector, it must be spilled to ensure it is visible to the collector. *)
  (* handled by adding interference edges between the call-live vector-typed variables and all the callee-save registers *)
  | ACallq (GlobalValue "collect", _, _) :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_directed_edges live_vars caller_save_aregisters map;
    let vec_live = get_live_vectors live_vars var_types in
    add_directed_edges vec_live callee_save_aregisters map;
    build_graph t (tail live_afters) map var_types

  (* Generic calls *)
  | ACallq (_, _, v) :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_directed_edges live_vars caller_save_aregisters map;
    add_bidirected_edges_if (fun e -> true) v live_vars map;
    build_graph t (tail live_afters) map var_types

  (* Otherwise *)
  | h :: t -> build_graph t (tail live_afters) map var_types
  | [] -> map

let rec build_defs defs =
  match defs with
  | LDefine (id, num_params, args, var_types, max_stack, lives, instrs) :: t ->
    let tbl = Hashtbl.create 10 in
    add_directed_edges args callee_save_aregisters tbl;
    let map = build_graph instrs lives tbl var_types in
    GDefine (id, num_params, args, var_types, max_stack, lives, map, instrs) :: build_defs t
  | [] -> []


let build_interference program : gprogram =
  match program with
  | LProgram (var_types, live_afters, datatype, defs, stmts) ->
    let new_defs = build_defs defs in
    let map = build_graph stmts live_afters (Hashtbl.create 10) var_types in
    (* print_interfer map; *)
    GProgram (var_types, live_afters, map, datatype, new_defs, stmts)
