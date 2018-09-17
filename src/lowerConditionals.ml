open AProgram

exception LowerConditionalsException of string

let lower_conditional_error s = raise (LowerConditionalsException s)

let gen_unique label cnt =
  cnt := !cnt + 1;
  label ^ (string_of_int !cnt)

let rec lower_instructions instrs uniq_cnt =
  match instrs with
  | [] -> []
  | AIf ((c, a1, a2), thn_instrs, _, els_instrs, _) :: tl ->
    let thn_label = gen_unique "thn" uniq_cnt in
    let end_label = gen_unique "end" uniq_cnt in
    Cmpq (a1, a2) :: JmpIf (c, thn_label) :: lower_instructions els_instrs uniq_cnt @
    Jmp end_label :: Label thn_label :: lower_instructions thn_instrs uniq_cnt @
    Label end_label :: lower_instructions tl uniq_cnt
  | h :: tl -> h :: lower_instructions tl uniq_cnt

let lower_conditionals program =
  match program with
  | GProgram (vars, interference, datatype, instrs) ->
    let uniq_count = ref 0 in
    let new_instrs = lower_instructions instrs uniq_count in
    GProgram (vars, interference, datatype, new_instrs)
