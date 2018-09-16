open AProgram

let is_deref arg = match arg with
  | Deref _ -> true
  | _ -> false

let rec patch_instrs instrs = match instrs with
  | [] -> []
  | Addq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Addq (Reg Rax, b) :: patch_instrs tl
    else Addq (a, b) :: patch_instrs tl
  | Subq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Subq (Reg Rax, b) :: patch_instrs tl
    else Subq (a, b) :: patch_instrs tl
  | Movq (a, b) :: tl ->
    if a = b then patch_instrs tl else
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch_instrs tl
    else Movq (a, b) :: patch_instrs tl
  | Xorq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Xorq (Reg Rax, b) :: patch_instrs tl
    else Xorq (a, b) :: patch_instrs tl
  | Movzbq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Movzbq (Reg Rax, b) :: patch_instrs tl
    else Movzbq (a, b) :: patch_instrs tl
  | Cmpq (a, b) :: tl ->
    (match b with
    | AInt _ -> Movq (b, Reg Rax) :: Cmpq (a, Reg Rax) :: patch_instrs tl
    | _ -> Cmpq (a, b) :: patch_instrs tl)
  | h :: tl -> h :: patch_instrs tl

let patch_instructions program = match program with
  | AProgram (var_space, datatype, instrs) ->
    let new_instrs = patch_instrs instrs in
    AProgram (var_space, datatype, new_instrs)
