open AProgram
open CProgram
open Registers
open List

exception SelectInstructionError of string
let select_instruction_error s = raise (SelectInstructionError s)

(* Store how many times we call collect for debugging purposes *)
let collect_call_count = ref 0

let get_collect_call_count () =
  collect_call_count := !collect_call_count + 1;
  AInt !collect_call_count

let select_exp e v : ainstr list =
  match e with
  | CArg a ->
    let arg = get_aarg_of_carg a in
    [Movq (arg, v)]
  | CPrint (dt, a) ->
    let arg = get_aarg_of_carg a in
    let prinstr = (match dt with
      | TypeInt -> "print_int"
      | TypeBool -> "print_bool"
      | TypeVoid -> "print_unit"
      | TypeVector l -> "print_vector"
    ) in
    [Movq (arg, Reg Rdi); Callq prinstr; Movq (Reg Rax, v)]
  | CRead ->
    [ACallq ("read_int", [], v)]
    (* [Callq "read_int"; Movq (Reg Rax, v)] *)
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
  | CAlloc (i, dt) ->
    [
      Movq (GlobalValue free_ptr, v);
      Addq (AInt (8 * (i + 1)), GlobalValue free_ptr);
      Movq (v, Reg Rax);
      Leaq (TypeRef dt, Reg Rcx);
      Movq (Reg Rcx, Deref (Rax, 0))
    ]
  | CVectorRef (ve, i) ->
    let varg = get_aarg_of_carg ve in
    [Movq (varg, Reg Rax); Movq (Deref (Rax, 8 * (i + 1)), v)]

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
  | CWhile (cnd, CCmp(o, l, r), thn) :: t ->
    let cmp = get_acmp_of_ccmp o in
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    let cndinstrs = select_stmts cnd in
    let thninstrs = select_stmts thn in
    AWhile (cndinstrs, [], (cmp, larg, rarg), thninstrs, []) :: select_stmts t
  | CWhile (cnd, _, thn) :: t -> select_instruction_error "select_stmt: While statement must use compare to true in condition"
  | CCollect i :: t ->
    ACallq ("collect", [root_stack_register; AInt i; get_collect_call_count ()], AVoid) :: select_stmts t
  | CVectorSet (ve, i, ne) :: t ->
    let varg = get_aarg_of_carg ve in
    let earg = get_aarg_of_carg ne in
    Movq (varg, Reg Rax) :: Movq (earg, Deref (Rax, 8 * (i + 1))) :: select_stmts t
| [] -> []

let select_instructions program : pprogram =
  match program with
  | CProgram (vars, datatype, stmts) ->
    PProgram (vars, datatype, select_stmts stmts)
