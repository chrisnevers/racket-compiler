open AProgram
open RProgram
open Registers
open Helper
open List

exception AssignHomesError of string
let assign_error msg = raise (AssignHomesError msg)

let get_register_offset arg homes offset =
  try
    Hashtbl.find homes arg
  with
  | Not_found ->
    offset := !offset - 8;
    Hashtbl.replace homes arg !offset;
    !offset

(* Map the variable to a register or spill to the stack if no space *)
let get_arg_home arg homes offset colors =
  match arg with
  | AVar v ->
    let index = Hashtbl.find colors arg in
    (* If no space, spill to stack *)
    if index >= num_of_registers then
      Deref (Rbp, get_register_offset arg homes offset)
    (* If no interference, put in any reg *)
    else if index = -1 then Reg Rbx
    (* Assign to corresponding register *)
    else Reg (List.nth registers index)
  | _ -> arg

let save_registers registers use_root_stack =
  let offset = ref 0 in
  let pushqs = List.map (fun r ->
    offset := !offset + 8;
    Pushq (r)
  ) registers
  in
  if !offset > 0 then pushqs @ [Subq (AInt !offset, Reg Rsp)] else []

let restore_registers registers use_root_stack =
  let offset = ref 0 in
  let popqs = List.map (fun r ->
    offset := !offset + 8;
    Popq (r)
  ) registers
  in
  if !offset > 0 then popqs @ [Addq (AInt !offset, Reg Rsp)] else []

let push_call_args registers =
  if List.length registers >= List.length arg_locations then
    assign_error "too many args to function"
  else List.mapi (fun i e -> Movq (e, List.nth arg_locations i)) registers

let get_arg_homes arg homes offset colors =
  List.map (fun e -> get_arg_home e homes offset colors) arg

let is_atomic vars live =
  List.filter (fun v -> match Hashtbl.find vars (get_avar_name v) with | TypeVector dt -> false | _ -> true) live

let is_ptr vars live =
  List.filter (fun v -> match Hashtbl.find vars (get_avar_name v) with | TypeVector dt -> true | _ -> false) live

let rec get_instrs instrs homes offset colors vars live_afters =
  match instrs with
  | [] -> []
  | Addq (a, b) :: tl ->
    let live_after = tail live_afters in
    Addq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Subq (a, b) :: tl ->
    let live_after = tail live_afters in
    Subq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Movq (a, b) :: tl ->
    let live_after = tail live_afters in
    Movq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Negq a :: tl ->
    let live_after = tail live_afters in
    Negq (get_arg_home a homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | ACallq (l, args, v) :: tl ->
    (* NEED HELP ... SOS PLZ SOME1 HELP ME *)
    let live_vars = hd live_afters in
    (* Get the variables that are atomic (: not vectors) and ptr (: vectors) *)
    let atomic_vars = is_atomic vars live_vars in
    let ptr_vars = is_ptr vars live_vars in
    (* Get the corresponding assigned homes (registers or derefs) *)
    let atomic_registers = get_arg_homes atomic_vars homes offset colors in
    let ptr_registers = get_arg_homes ptr_vars homes offset colors in
    (* Only save atomic registers that are caller save *)
    let save_atomic_regs = List.filter (fun e -> List.mem e caller_save_aregisters) atomic_registers in
    (* ??? What to do with ptr registers and root stack ??? *)
    (* Before calling label, save the atomic and ptr registers *)
    save_registers save_atomic_regs false @
    save_registers ptr_registers true @
    (* Map needed args to the necessary func arg locations *)
    push_call_args (get_arg_homes args homes offset colors) @
    (* Call the function *)
    Callq l ::
    (* Move result to variable *)
    Movq (Reg Rax, get_arg_home v homes offset colors) ::
    (* Pop the needed live variables back off the stack *)
    restore_registers save_atomic_regs false @
    restore_registers ptr_registers true @
    (* Get rest of instructions *)
    get_instrs tl homes offset colors vars (tail live_afters)
  | Callq l :: tl ->
    let live_after = tail live_afters in
    Callq l :: (get_instrs tl homes offset colors vars live_after)
  | Pushq a :: tl ->
    let live_after = tail live_afters in
    Pushq (get_arg_home a homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Popq a :: tl ->
    let live_after = tail live_afters in
    Popq (get_arg_home a homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Xorq (a, b) :: tl ->
    let live_after = tail live_afters in
    Xorq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Cmpq (a, b) :: tl ->
    let live_after = tail live_afters in
    Cmpq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Movzbq (a, b) :: tl ->
    let live_after = tail live_afters in
    Movzbq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Set (c, a) :: tl ->
    let live_after = tail live_afters in
    Set (c, get_arg_home a homes offset colors) :: (get_instrs tl homes offset colors vars live_after)
  | Retq :: tl ->
    Retq :: (get_instrs tl homes offset colors vars  (tail live_afters))
  | Jmp l :: tl ->
    Jmp l:: (get_instrs tl homes offset colors vars  (tail live_afters))
  | JmpIf (c, l) :: tl ->
    JmpIf (c, l):: (get_instrs tl homes offset colors vars  (tail live_afters))
  | Label l :: tl ->
    Label l :: (get_instrs tl homes offset colors vars  (tail live_afters))
  | AWhile (cnd_instrs, _, (c, a, b), thn_instrs, _) :: tl -> assign_error "while should not be in assign homes"
  | AIf ((c, a, b), thn_instrs, _, els_instrs, _) :: tl -> assign_error "if should not be in assign homes"
  | Leaq (a, b) :: tl -> Leaq (get_arg_home a homes offset colors, get_arg_home b homes offset colors) :: (get_instrs tl homes offset colors vars (tail live_afters))

let assign_homes program =
  match program with
  | GCProgram (vars, live_afters, colors, datatype, instrs) ->
    let homes = Hashtbl.create 10 in
    let offset = ref 0 in
    let new_instrs = get_instrs instrs homes offset colors vars live_afters in
    let var_space = make_multiple_of_16 (- !offset) in
    AProgram (var_space, datatype, new_instrs)
