open AProgram

let is_deref arg = match arg with
  | DerefVar _ -> true
  | Deref _ -> true
  | GlobalValue _ -> true
  | TypeRef _ -> true
  | _ -> false

let is_void arg = match arg with
  | AVoid -> true
  | _ -> false

let is_int arg = match arg with
  | AInt _ -> true
  | _ -> false

let rec patch_instrs instrs = match instrs with
  | [] -> []
  | IDivq s :: tl ->
    if is_int s then Movq (s, Reg Rcx) :: IDivq (Reg Rcx) :: patch_instrs tl
    else IDivq s :: patch_instrs tl
  | IMulq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: IMulq (Reg Rax, b) :: patch_instrs tl
    else IMulq (a, b) :: patch_instrs tl
  | Addq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Addq (Reg Rax, b) :: patch_instrs tl
    else Addq (a, b) :: patch_instrs tl
  | Subq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Subq (Reg Rax, b) :: patch_instrs tl
    else Subq (a, b) :: patch_instrs tl
  | Movq (a, b) :: tl ->
    if is_void b then patch_instrs tl else
    if is_void a then Movq (AInt 0, b) :: patch_instrs tl else
    if a = b then patch_instrs tl else
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch_instrs tl
    else Movq (a, b) :: patch_instrs tl
  | Xorq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Xorq (Reg Rax, b) :: patch_instrs tl
    else Xorq (a, b) :: patch_instrs tl
  | Movzbq (a, b) :: tl ->
    if is_deref b then
      Movzbq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch_instrs tl
    else Movzbq (a, b) :: patch_instrs tl
  | Cmpq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Cmpq (Reg Rax, b) :: patch_instrs tl
    else if is_int a && is_int b then
      Movq (a, Reg Rax) :: Movq (b, Reg Rcx) ::Cmpq (Reg Rax, Reg Rcx) :: patch_instrs tl
    else if is_int a then
      Movq (a, Reg Rax) :: Cmpq (Reg Rax, b) :: patch_instrs tl
    else if is_int b then
      Movq (b, Reg Rax) :: Cmpq (Reg Rax, a) :: patch_instrs tl
    else Cmpq (a, b) :: patch_instrs tl
  | Leaq (a, b) :: tl ->
    if is_deref b then
      Leaq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch_instrs tl
    else Leaq (a, b) :: patch_instrs tl
  | h :: tl -> h :: patch_instrs tl

let rec patch_defs defs =
  match defs with
  | ADefine (id, num_params, vars, var_types, max_stack, vec_space, instrs) :: t ->
    let new_def = ADefine (id, num_params, vars, var_types, max_stack, vec_space, patch_instrs instrs) in
    new_def :: patch_defs t
  | [] -> []

let patch_instructions program = match program with
  | AProgram (var_space, rootstack_space, datatype, defs, instrs) ->
    let new_defs = patch_defs defs in
    let new_instrs = patch_instrs instrs in
    AProgram (var_space, rootstack_space, datatype, new_defs, new_instrs)
