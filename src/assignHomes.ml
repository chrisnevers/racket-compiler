open AProgram

let make_multiple_of_16 i =
  let remainder = i mod 16 in
  if remainder = 0 then i
  else i + (16 - remainder)

let get_register_offset arg homes offset =
  try
    Hashtbl.find homes arg
  with
  | Not_found ->
    offset := !offset - 8;
    Hashtbl.replace homes arg !offset;
    !offset

let get_arg_home arg homes offset =
  match arg with
  | AVar v -> Deref (Rbp, get_register_offset arg homes offset)
  | _ -> arg

let rec get_instrs instrs homes offset =
  match instrs with
  | [] -> []
  | Addq (a, b) :: tail ->
    Addq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Subq (a, b) :: tail ->
    Subq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Movq (a, b) :: tail ->
    Movq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Negq a :: tail ->
    Negq (get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Callq l :: tail ->
    Callq l :: (get_instrs tail homes offset)
  | Pushq a :: tail ->
    Pushq (get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Popq a :: tail ->
    Popq (get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Xorq (a, b) :: tail ->
    Xorq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Cmpq (a, b) :: tail ->
    Cmpq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Movzbq (a, b) :: tail ->
    Movzbq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Set (c, a) :: tail ->
    Set (c, get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Retq :: tail ->
    Retq :: (get_instrs tail homes offset)
  | Jmp l :: tail ->
    Jmp l:: (get_instrs tail homes offset)
  | JmpIf (c, l) :: tail ->
    JmpIf (c, l):: (get_instrs tail homes offset)
  | Label l :: tail ->
    Label l :: (get_instrs tail homes offset)
  | AIf ((c, a, b), thn_instrs, _, els_instrs, _) :: tail ->
    AIf (
      (c, get_arg_home a homes offset, get_arg_home b homes offset),
      (get_instrs thn_instrs homes offset), [],
      (get_instrs els_instrs homes offset), []) :: (get_instrs tail homes offset)

let assign_homes program =
  match program with
  | GProgram (vars, graph, datatype, instrs) ->
    let homes = Hashtbl.create 10 in
    let offset = ref 0 in
    let new_instrs = get_instrs instrs homes offset in
    let var_space = make_multiple_of_16 (- !offset) in
    AProgram (var_space, datatype, new_instrs)
