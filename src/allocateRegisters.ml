open AProgram
open Registers
open Helper
open List

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
  (car !current)

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
    (* Find its neighboring nodes *)
    let adjacents = Hashtbl.find graph max_saturated in
    (* Find what its neighboring nodes are already assigned *)
    let adjacent_colors = get_adjacent_colors colors adjacents in
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

let color_graph graph vars move =
  (* List of numbers a variable cannot be assigned *)
  let saturations = create_graph vars [] in
  (* The color (number) a variable is assigned *)
  let colors = create_graph vars (-1) in
  get_colors (Hashtbl.copy graph) saturations colors move

(* Map the variable to a register or spill to the stack if no space *)
let get_register a graph =
  match a with
  | AVar v ->
    let index = Hashtbl.find graph a in
    if index >= num_of_registers then a
    else if index = -1 then Reg Rbx
    else Reg (List.nth registers index)
  | _ -> a

let rec get_new_instrs instrs graph =
  match instrs with
  | [] -> []
  | Addq (a, b) :: tl ->
    Addq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Subq (a, b) :: tl ->
    Subq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Movq (a, b) :: tl ->
    let a_register = get_register a graph in
    let b_register = get_register b graph in
    if (a_register = b_register) then get_new_instrs tl graph
    else Movq (a_register, b_register) :: get_new_instrs tl graph
  | Xorq (a, b) :: tl ->
    Xorq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Cmpq (a, b) :: tl ->
    Cmpq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Movzbq (a, b) :: tl ->
    Movzbq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Set (c, b) :: tl ->
    Set (c, get_register b graph) :: get_new_instrs tl graph
  | Jmp l :: tl ->
    Jmp l :: get_new_instrs tl graph
  | JmpIf (c, l) :: tl ->
    JmpIf (c, l) :: get_new_instrs tl graph
  | Label l :: tl ->
    Label l :: get_new_instrs tl graph
  | Negq a :: tl ->
    Negq (get_register a graph) :: get_new_instrs tl graph
  | Callq l :: tl ->
    Callq l :: get_new_instrs tl graph
  | Pushq a :: tl ->
    Pushq (get_register a graph) :: get_new_instrs tl graph
  | Popq a :: tl ->
    Popq (get_register a graph) :: get_new_instrs tl graph
  | Retq :: tl ->
    Retq :: get_new_instrs tl graph
  | AIf ((c, a, b), thn_instr, _, els_instr, _):: tl ->
    AIf ((c, get_register a graph, get_register b graph),
          get_new_instrs thn_instr graph, [],
          get_new_instrs els_instr graph, []) :: get_new_instrs tl graph
  | AWhile (cnd_instr, _, (c, a, b), thn_instr, _):: tl ->
    AWhile (get_new_instrs cnd_instr graph, [],
      (c, get_register a graph, get_register b graph),
      get_new_instrs thn_instr graph, []) :: get_new_instrs tl graph

let rec create_move_bias_graph instrs tbl =
  match instrs with
  | Movq (s, d) :: tl when is_var s && is_var d ->
    append_to_value s d tbl;
    append_to_value d s tbl;
    create_move_bias_graph tl tbl
  | _ :: tl -> create_move_bias_graph tl tbl
  | [] -> tbl

let allocate_registers program : gprogram =
  match program with
  | GProgram (vars, graph, datatype, instrs) ->
    (* Create move bias graph to doc which vars are movq'd to other vars *)
    let move = create_move_bias_graph instrs (Hashtbl.create 10) in
    (* Assign each var a color unique to its adjacent nodes *)
    let colors = color_graph graph vars move in
    (* Reiterate over instructions & replace vars with registers *)
    let new_instrs = get_new_instrs instrs colors in
    GProgram (vars, graph, datatype, new_instrs)
