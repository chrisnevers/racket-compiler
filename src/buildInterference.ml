open AProgram
open List

let find_in_map key map = 
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map =
  let current_value = find_in_map key map in
  Hashtbl.remove map key;
  Hashtbl.add map key (List.sort_uniq compare (value :: current_value))

let add_bidirected_edge n1 n2 map =
  append_to_value n1 n2 map;
  append_to_value n2 n1 map

let rec add_edges cnd (d: aarg) (targets: string list) map = 
  match targets with
  | n :: t ->
    if cnd (AVar n) then (add_bidirected_edge d (AVar n) map; add_edges cnd d t map)
    else add_edges cnd d t map
  | [] -> ()

let rec add_edges_from_nodes nodes targets map =
  match nodes with
  | h :: t ->
    add_edges (fun v -> true) (register_of_string h) targets map;
    add_edges_from_nodes t targets map
  | [] -> ()

let rec build_graph stmts live_afters map =
  match stmts with
  | Movq (s, d) :: t ->
    (* add the edge (d, v) for every v of Lafter(k) unless v = d or v = s. *)
    let live_vars = hd (live_afters) in
    add_edges (fun v -> v <> d && v <> s) d live_vars map;
    build_graph t (tl live_afters) map
  | Addq (s, d) :: t | Subq (s, d) :: t ->
    (* add the edge (d, v) for every v of Lafter(k) unless v = d. *)
    let live_vars = hd (live_afters) in
    add_edges (fun v -> v <> d) d live_vars map;
    build_graph t (tl live_afters) map
  | Callq label :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    let live_vars = hd (live_afters) in
    add_edges_from_nodes caller_save_registers live_vars map;
    build_graph t (tl live_afters) map
  | h :: t -> build_graph t (tl live_afters) map
  | [] -> map

let build_interference program : gprogram =
  match program with
  | LProgram (vars, live_afters, datatype, stmts) ->
    let map = build_graph stmts live_afters (Hashtbl.create 10) in
    GProgram (vars, map, datatype, stmts)
