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

let rec add_edges cnd (d: aarg) (targets: aarg list) map =
  match targets with
  | n :: t ->
    if cnd n then (add_bidirected_edge d n map; add_edges cnd d t map)
    else add_edges cnd d t map
  | [] -> ()

let rec add_edges_from_nodes nodes targets map =
  match nodes with
  | h :: t ->
    List.iter (fun e -> append_to_value h e map) targets;
    add_edges_from_nodes t targets map
  | [] -> ()

let rec build_graph stmts live_afters map var_types : interference =
  match stmts with
  | Movq (s, d) :: t | Movzbq(s, d) :: t ->
    let live_vars = hd (live_afters) in
    (* add the edge (d, v) for every v of Lafter(k) unless v = d or v = s. *)
    add_edges (fun v -> v <> d && v <> s) d live_vars map;
    build_graph t (tail live_afters) map var_types
  | Addq (s, d) :: t | Subq (s, d) :: t (* | XOrq :: t ? *)->
    let live_vars = hd (live_afters) in
    (* add the edge (d, v) for every v of Lafter(k) unless v = d. *)
    add_edges (fun v -> v <> d) d live_vars map;
    build_graph t (tail live_afters) map var_types
  | Callq "collect" :: t | ACallq ("collect", _, _) :: t ->
    let live_vars = hd (live_afters) in
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_edges_from_nodes live_vars caller_save_aregisters map;
    (* if a vector-typed variable is live during a call to the collector, it must be spilled to ensure it is visible to the collector. *)
    (* handled by adding interference edges between the call-live vector-typed variables and all the callee-save registers *)
    let vec_live = List.filter (fun v ->
      match Hashtbl.find var_types (get_avar_name v) with
      | TypeVector _ -> true
      | _ -> false
    ) live_vars in
    add_edges_from_nodes vec_live callee_save_aregisters map;
    build_graph t (tail live_afters) map var_types
  | Callq _ :: t ->
    let live_vars = hd (live_afters) in
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_edges_from_nodes live_vars caller_save_aregisters map;
    build_graph t (tail live_afters) map var_types
  | ACallq (_, _, v) :: t ->
    let live_vars = hd (live_afters) in
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_edges_from_nodes live_vars caller_save_aregisters map;
    add_edges (fun e -> true) v live_vars map;
    build_graph t (tail live_afters) map var_types
  | AIf ((c, s, d), thn_instrs, thn_lafter, els_instrs, els_lafter) :: t ->
    let _ = build_graph thn_instrs thn_lafter map var_types in
    let _ = build_graph els_instrs els_lafter map var_types in
    build_graph t (tail live_afters) map var_types
  | AWhile (cnd_instrs, cnd_lafter, (c, s, d), thn_instrs, thn_lafter) :: t ->
    let _ = build_graph cnd_instrs cnd_lafter map var_types in
    let _ = build_graph thn_instrs thn_lafter map var_types in
    build_graph t (tail live_afters) map var_types
  | h :: t -> build_graph t (tail live_afters) map var_types
  | [] -> map

let build_interference program : gprogram =
  match program with
  | LProgram (var_types, live_afters, datatype, stmts) ->
    let map = build_graph stmts live_afters (Hashtbl.create 10) var_types in
    GProgram (var_types, live_afters, map, datatype, stmts)
