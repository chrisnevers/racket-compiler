open AProgram
open RProgram
open Registers
open Helper
open List

exception AssignHomesError of string
let assign_error msg = raise (AssignHomesError msg)

let get_register_offset arg homes stack_offset =
  try
    Hashtbl.find homes arg
  with
  | Not_found ->
    stack_offset := !stack_offset - 8;
    Hashtbl.replace homes arg !stack_offset;
    !stack_offset

(* Map the variable to a register or spill to the stack if no space *)
let get_arg_home arg homes stack_offset colors vars rootstack_offset =
  match arg with
  | AVar v ->
    let index = Hashtbl.find colors arg in
    (* If no space, spill to stack *)
    if index >= num_of_registers then
      (* If vector store to rootstack *)
      match Hashtbl.find vars (get_avar_name arg) with
      | TypeVector _ -> Deref (root_stack_register, get_register_offset arg homes rootstack_offset)
      | _ -> Deref (Rbp, get_register_offset arg homes stack_offset)
    (* If no interference, put in any reg *)
    else if index = -1 then Reg Rbx
    (* Assign to corresponding register *)
    else Reg (List.nth registers index)
  | TypeRef d -> arg
  | _ -> arg

let save_registers registers =
  let offset = ref 0 in
  let pushqs = List.map (fun r ->
    offset := !offset + 8;
    Pushq (r)
  ) registers
  in
  if !offset > 0 then pushqs @ [Subq (AInt !offset, Reg Rsp)] else []

let restore_registers registers =
  let offset = ref 0 in
  let popqs = List.map (fun r ->
    offset := !offset + 8;
    Popq (r)
  ) registers
  in
  if !offset > 0 then Addq (AInt !offset, Reg Rsp) :: popqs else []

let save_ptr_registers registers =
  let offset = ref (- 8) in
  let pushqs = List.map (fun r ->
    offset := !offset + 8;
    Movq (r, Deref (root_stack_register, !offset))
  ) registers
  in
  if !offset > (- 8) then pushqs else []

let restore_ptr_registers registers =
  let offset = ref (- 8) in
  let pushqs = List.map (fun r ->
    offset := !offset + 8;
    Movq (Deref (root_stack_register, !offset), r)
  ) registers
  in
  if !offset > (- 8) then pushqs else []

let push_call_args registers =
  if List.length registers >= List.length arg_locations then
    assign_error "too many args to function"
  else List.mapi (fun i e -> Movq (e, List.nth arg_locations i)) registers

let get_arg_homes arg homes offset colors vars rootstack_offset=
  List.map (fun e -> get_arg_home e homes offset colors vars rootstack_offset) arg

let is_atomic vars live =
  List.filter (fun v -> match Hashtbl.find vars (get_avar_name v) with | TypeVector dt -> false | _ -> true) live

let is_ptr vars live =
  List.filter (fun v -> match Hashtbl.find vars (get_avar_name v) with | TypeVector dt -> true | _ -> false) live

let rec get_instrs instrs homes offset colors live_afters vars rootstack_offset =
  match instrs with
  | [] -> []
  | Addq (a, b) :: t ->
    Addq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Subq (a, b) :: t ->
    Subq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Movq (a, b) :: t ->
    Movq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Negq a :: t ->
    Negq (get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | ACallq (l, args, v) :: t ->
    (* NEED HELP ... SOS PLZ SOME1 HELP ME *)
    let live_vars = hd live_afters in
    (* Get the variables that are atomic (: not vectors) and ptr (: vectors) *)
    let atomic_vars = is_atomic vars live_vars in
    let ptr_vars = is_ptr vars live_vars in
    (* Get the corresponding assigned homes (registers or derefs) *)
    let atomic_registers = get_arg_homes atomic_vars homes offset colors vars rootstack_offset in
    let ptr_registers = get_arg_homes ptr_vars homes offset colors vars rootstack_offset in
    rootstack_offset := if List.length ptr_registers > !rootstack_offset then List.length ptr_registers else !rootstack_offset;
    (* Only save atomic registers that are caller save *)
    let save_atomic_regs = List.filter (fun e -> List.mem e caller_save_aregisters) atomic_registers in
    (* Before calling label, save the atomic and ptr registers *)
    save_registers save_atomic_regs @
    save_ptr_registers ptr_registers @
    (* Map needed args to the necessary func arg locations *)
    push_call_args (get_arg_homes args homes offset colors vars rootstack_offset) @
    (* Call the function *)
    Callq l ::
    (* Move result to variable *)
    Movq (Reg Rax, get_arg_home v homes offset colors vars rootstack_offset) ::
    (* Pop the needed live variables back off the stack *)
    restore_registers save_atomic_regs @
    restore_ptr_registers ptr_registers @
    (* Get rest of instructions *)
    get_instrs t homes offset colors (tl live_afters) vars rootstack_offset
  | Callq l :: t ->
    Callq l :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Pushq a :: t ->
    Pushq (get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Popq a :: t ->
    Popq (get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Xorq (a, b) :: t ->
    Xorq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Cmpq (a, b) :: t ->
    Cmpq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Movzbq (a, b) :: t ->
    Movzbq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Set (c, a) :: t ->
    Set (c, get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Retq :: t ->
    Retq :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Jmp l :: t ->
    Jmp l:: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | JmpIf (c, l) :: t ->
    JmpIf (c, l):: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Label l :: t ->
    Label l :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | AWhile (cnd_instrs, cnd_live_afters, (c, a, b), thn_instrs, thn_live_afters) :: t ->
    let new_cnd_instrs = get_instrs cnd_instrs homes offset colors cnd_live_afters vars rootstack_offset in
    let ahome = get_arg_home a homes offset colors vars rootstack_offset in
    let bhome = get_arg_home b homes offset colors vars rootstack_offset in
    let new_thn_instrs = get_instrs thn_instrs homes offset colors thn_live_afters vars rootstack_offset in
    AWhile (new_cnd_instrs, [], (c, ahome, bhome), new_thn_instrs, []) :: (get_instrs t homes offset colors (tl live_afters) vars) rootstack_offset
    (* assign_error "while should not be in assign homes" *)
  | AIf ((c, a, b), thn_instrs, thn_live_afters, els_instrs, els_live_afters) :: t ->
    let ahome = get_arg_home a homes offset colors vars rootstack_offset in
    let bhome = get_arg_home b homes offset colors vars rootstack_offset in
    let new_thn_instrs = get_instrs thn_instrs homes offset colors thn_live_afters vars rootstack_offset in
    let new_els_instrs = get_instrs els_instrs homes offset colors els_live_afters vars rootstack_offset in
    AIf ((c, ahome, bhome), new_thn_instrs, [], new_els_instrs, []) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Leaq (a, b) :: t -> Leaq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)

let assign_homes program =
  match program with
  | GCProgram (vars, live_afters, colors, datatype, instrs) ->
    let homes = Hashtbl.create 10 in
    let stack_offset = ref 0 in
    let rootstack_offset = ref 0 in
    let new_instrs = get_instrs instrs homes stack_offset colors live_afters vars rootstack_offset in
    let var_space = make_multiple_of_16 (- !stack_offset) in
    let vec_space = make_multiple_of_16 (!rootstack_offset) in
    AProgram (var_space, vec_space, datatype, new_instrs)
