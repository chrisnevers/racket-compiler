open AProgram
open RProgram
open Registers
open AssignHomes
open List

let gen_unique label cnt =
  cnt := !cnt + 1;
  label ^ (string_of_int !cnt)

let rec lower_instructions instrs uniq_cnt =
  match instrs with
  | [] -> []
  | AIf ((c, a1, a2), thn_instrs, _, els_instrs, _) :: t ->
    let thn_label = gen_unique "thn" uniq_cnt in
    let end_label = gen_unique "end" uniq_cnt in
    Cmpq (a1, a2) :: JmpIf (c, thn_label) :: lower_instructions els_instrs uniq_cnt @
    Jmp end_label :: Label thn_label :: lower_instructions thn_instrs uniq_cnt @
    Label end_label :: lower_instructions t uniq_cnt
  | AWhile (cnd_instrs, _, (c, a1, a2), thn_instrs, _) :: t ->
    let while_label = gen_unique "while" uniq_cnt in
    let end_label = gen_unique "end" uniq_cnt in
    Label while_label :: lower_instructions cnd_instrs uniq_cnt @ Cmpq (a1, a2) :: JmpIf (get_opposite_cmp c, end_label)
    :: lower_instructions thn_instrs uniq_cnt @ Jmp while_label
    :: Label end_label :: lower_instructions t uniq_cnt
  | h :: t -> h :: lower_instructions t uniq_cnt

let rec lower_defs defs uniq_count =
  match defs with
  | ADefine (id, num_params, vars, var_types, max_stack, vec_space, instrs) :: t ->
    let new_instrs = lower_instructions instrs uniq_count in
    ADefine (id, num_params, vars, var_types, max_stack, vec_space, new_instrs) :: lower_defs t uniq_count
  | [] -> []

let lower_conditionals program =
  match program with
  | AProgram (var_space, rootstack_space, datatype, defs, instrs) ->
    let uniq_count = ref 0 in
    let new_defs = lower_defs defs uniq_count in
    let new_instrs = lower_instructions instrs uniq_count in
    AProgram (var_space, rootstack_space, datatype, new_defs, new_instrs)
