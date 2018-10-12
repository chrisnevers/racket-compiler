open AProgram
open Registers
open Helper
open List

exception AllocateRegistersException of string
let allocate_error msg = raise (AllocateRegistersException msg)

let find_in_map key map =
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map =
  let current_value = find_in_map key map in
  Hashtbl.replace map key (List.sort_uniq compare (value :: current_value))

let create_graph keys value =
  let graph = Hashtbl.create 10 in
  let rec add_to_table ks =
    match ks with
    | key :: tail -> Hashtbl.replace graph (AVar key) value; add_to_table tail
    | [] -> ()
  in
  let _ = add_to_table keys in graph

let rec get_most_saturated graph saturations =
  let current = ref (AVar "", 0) in
  Hashtbl.iter (fun k v ->
    let no_of_saturations = List.length (find_in_map k saturations) in
    if no_of_saturations >= (cdr !current) && (is_var k) then
      current := (k, no_of_saturations)) graph;
  if (car !current) == AVar "" then
    allocate_error "Could not find max saturated node"
  else (car !current)

let rec get_lowest_color adjacent_colors cur =
  match adjacent_colors with
  | h :: t ->
    if h != -1 && h = cur then
      get_lowest_color t (cur + 1)
    else
      get_lowest_color t cur
  | [] -> cur

let rec add_color_to_saturations saturations adjacents color =
  match adjacents with
  | h :: t ->
    append_to_value h color saturations;
    add_color_to_saturations saturations t color
  | [] -> ()

let get_adjacent_colors colors adjacents =
  List.sort compare (List.map (fun e -> Hashtbl.find colors e) adjacents)

let rec get_colors graph saturations colors move =
  match Hashtbl.length graph with
  | 0 -> colors
  | _ ->
    (* Pick node in graph with highest saturation *)
    let max_saturated = get_most_saturated graph saturations in
    try
    (* Find its neighboring nodes *)
    let adjacents = Hashtbl.find graph max_saturated in
    (* Find what its neighboring nodes are already assigned *)
    (* Get lowest color using saturations not adjacents. This way registers are put into account *)
    let adjacent_colors = Hashtbl.find saturations max_saturated in
    (* Find its move bias neighboring nodes *)
    let move_adjacents = find_in_map max_saturated move in
    (* Find whats its move bias neighbors are already assigned *)
    let bias_colors = List.filter (fun e -> e != (-1) && not (List.mem e adjacent_colors))
                      (get_adjacent_colors colors move_adjacents) in
    (* Pick lowest number not in neighboring nodes *)
    let lowest_color = if bias_colors = [] then get_lowest_color adjacent_colors 0 else hd bias_colors in
    (* Add chosen color to final color map *)
    Hashtbl.replace colors max_saturated lowest_color;
    add_color_to_saturations saturations adjacents lowest_color;
    (* Remove node from processing list *)
    Hashtbl.remove graph max_saturated;
    get_colors graph saturations colors move
  with Not_found -> allocate_error ("Could not find max_saturated node in graph: " ^ (string_of_aarg max_saturated))

(* Add register colors to the saturation list of any variable live during a callq *)
let add_register_saturations sat graph =
  (* Get index of given register in the set of all assignable registers *)
  let rec get_index e l cnt =
    match l with
    | h :: t -> if h = e then cnt else get_index e t (cnt + 1)
    | [] -> -1
  in
  (* Gets the equivalent color of a register *)
  let get_register_colors k graph : int list =
    try
      let values = Hashtbl.find graph k in
      List.rev (List.fold_left (fun acc e ->
        match e with
        | Reg r ->
          let index = get_index r registers 0 in
          if index != -1 then index :: acc else acc
        | _ -> acc
      ) [] values)
    with Not_found -> []
  in
  (* Any variable that interfers with registers, add those register colors to its saturation list *)
  Hashtbl.iter (fun k v -> Hashtbl.replace sat k (get_register_colors k graph)) sat;
  sat

let print_saturation_graph saturations =
  Hashtbl.iter (fun k v ->
    print_string ((string_of_aarg k) ^ ": [");
    print_string (List.fold_left (fun acc e -> acc ^ (string_of_int e) ^ " ") "" v);
    print_endline "]";
  ) saturations

(* Removes registers (added during callq - build interference) from working set *)
let remove_registers map =
  Hashtbl.filter_map_inplace (fun k v ->
    if not (is_var k) then None
    (* Remove anything thats not a variable from the interference graph value *)
    else Some (List.filter (fun e ->
      match e with
      | AVar _ -> true
      | _ -> false
    ) v)
  ) map;
  map

let color_graph graph vars move =
  (* List of numbers a variable cannot be assigned *)
  let saturations = add_register_saturations (create_graph vars []) graph in
  (* print_saturation_graph saturations; *)
  (* The color (number) a variable is assigned *)
  let colors = create_graph vars (-1) in
  get_colors (remove_registers (Hashtbl.copy graph)) saturations colors move

let rec create_move_bias_graph instrs tbl =
  match instrs with
  | Movq (s, d) :: tl when is_var s && is_var d ->
    append_to_value s d tbl;
    append_to_value d s tbl;
    create_move_bias_graph tl tbl
  | _ :: tl -> create_move_bias_graph tl tbl
  | [] -> tbl

let allocate_registers program : gcprogram =
  match program with
  | GProgram (vars, live_afters, graph, datatype, instrs) ->
    let jvars = get_hashtable_keys vars in
    (* Create move bias graph to doc which vars are movq'd to other vars *)
    let move = create_move_bias_graph instrs (Hashtbl.create 10) in
    (* Assign each var a color unique to its adjacent nodes *)
    (* print_gprogram (GProgram (vars, live_afters, remove_registers (Hashtbl.copy graph), datatype, instrs)); *)
    let colors = color_graph graph jvars move in
    (* Reiterate over instructions & replace vars with registers *)
    (* print_gprogram (GProgram (vars, live_afters, graph, datatype, instrs)); *)
    (* print_color_graph colors; *)
    (* let new_instrs = get_new_instrs instrs colors in *)
    GCProgram (vars, live_afters, colors, datatype, instrs)
