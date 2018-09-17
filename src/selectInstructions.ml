open AProgram
open CProgram

exception SelectInstructionError of string
let select_instruction_error s = raise (SelectInstructionError s)

let select_exp e v : ainstr list =
  match e with
  | CArg a ->
    let arg = get_aarg_of_carg a in
    [Movq (arg, v)]
  | CRead ->
    [Callq "read_int"; Movq (Reg Rax, v)]
  | CUnOp (o, a) ->
    let arg = get_aarg_of_carg a in
    if arg = v then [Negq v] else [Movq (arg, v); Negq v]
  | CBinOp ("+", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    if larg = v then [Addq (rarg, v)] else
    if rarg = v then [Addq (larg, v)] else
    [Movq (larg, v); Addq (rarg, v)]
  | CBinOp (_, _, _) ->
    select_instruction_error "select_exp: Unsupported binary arithmetic operator"
  | CNot a ->
    let arg = get_aarg_of_carg a in
    [Movq (arg, v); Xorq (AInt 1, v)]
  | CCmp (o, l, r) ->
    let op = get_acmp_of_ccmp o in
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    (* Handle switching cmpq arg positions *)
    [Cmpq (rarg, larg); Set (op, ByteReg Al); Movzbq (ByteReg Al, v)]

let rec select_stmts stmt : ainstr list =
  match stmt with
  | CAssign (v, e) :: t ->
    select_exp e (AVar v) @ select_stmts t
  | CReturn a :: t ->
    let arg = get_aarg_of_carg a in
    Movq (arg, Reg Rax) :: select_stmts t
  | CIf (CCmp(o, l, r), thn, els) :: t ->
    let cmp = get_acmp_of_ccmp o in
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    let thninstrs = select_stmts thn in
    let elsinstrs = select_stmts els in
    AIf ((cmp, larg, rarg), thninstrs, [], elsinstrs, []) :: select_stmts t
  | CIf (_, thn, els) :: t ->
    select_instruction_error "select_stmt: If statement must use compare to true in condition"
  | [] -> []

let select_instructions program : pprogram =
  match program with
  | CProgram (vars, datatype, stmts) ->
    PProgram (vars, datatype, select_stmts stmts)
