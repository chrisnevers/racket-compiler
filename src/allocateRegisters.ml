open AProgram

let cdr = fun (_, b) -> b
let car = fun (a, _) -> a

let find_in_map key map = 
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map =
  let current_value = find_in_map key map in
  Hashtbl.remove map key;
  Hashtbl.add map key (List.sort_uniq compare (value :: current_value))
  
let create_graph keys value =
  let graph = Hashtbl.create 10 in
  let rec add_to_table ks =
    match ks with
    | key :: tail -> Hashtbl.add graph (AVar key) value; add_to_table tail
    | [] -> ()
  in
  let _ = add_to_table keys in graph
  
let rec get_most_saturated saturations =
  let current = ref (AVar "", 0) in
  Hashtbl.iter (fun k v -> if List.length v >= (cdr !current) then current := (k, List.length v)) saturations;
  (car !current)

let rec get_lowest_color adjacent_colors cur =
  match adjacent_colors with
  | h :: t ->
    if h != -1 && h = cur then 
      get_lowest_color t (cur + 1) 
    else cur
  | [] -> cur

let rec add_color_to_saturations saturations adjacents color =
  match adjacents with
  | h :: t ->
    append_to_value h color saturations;
    add_color_to_saturations saturations t color
  | [] -> ()

let rec get_adjacent_colors colors adjacents =
  match adjacents with
  | h :: t ->
    Hashtbl.find colors h :: (get_adjacent_colors colors t)
  | [] -> []

let rec get_colors graph saturations colors = 
  match Hashtbl.length graph with
  | 0 -> ()
  | _ ->
      let max_saturated = get_most_saturated saturations in
      let adjacents= Hashtbl.find graph max_saturated in
      let adjacent_colors = List.sort compare (get_adjacent_colors colors adjacents) in
      let lowest_color = get_lowest_color adjacent_colors 0 in
      Hashtbl.add colors max_saturated lowest_color;
      add_color_to_saturations saturations adjacents lowest_color;
      Hashtbl.remove graph max_saturated;
      get_colors graph saturations colors      

let color_graph graph vars =
  (* List of numbers a variable cannot be assigned *)
  let saturations = create_graph vars [] in
  (* The number a variable is assigned *)
  let colors = create_graph vars (-1) in
  get_colors (Hashtbl.copy graph) saturations colors
  (* Colors map now has unique var mappings *)

let allocate_registers program : gprogram =
  match program with
  | GProgram (vars, graph, datatype, instrs) ->
    color_graph graph vars;
    GProgram (vars, graph, datatype, instrs)
