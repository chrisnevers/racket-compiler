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
    (* Args: value, ?tag, newline *)
    let prinstrs = (match dt with
      | TypeInt -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_int"]
      | TypeBool -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_bool"]
      | TypeVoid -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_void"]
      | TypeVector l -> [Movq (arg, Reg Rdi); Leaq (TypeRef dt, Reg Rsi); Movq (AInt 1, Reg Rdx); Callq "print_vector"]
    ) in
    prinstrs
  | CRead ->
    [ACallq ("read_int", [], v)]
  | CUnOp (o, a) ->
    let arg = get_aarg_of_carg a in
    if arg = v then [Negq v] else [Movq (arg, v); Negq v]
  | CBinOp ("+", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    if larg = v then [Addq (rarg, v)] else
    if rarg = v then [Addq (larg, v)] else
    [Movq (larg, v); Addq (rarg, v)]
  | CBinOp ("*", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    if larg = v then [IMulq (rarg, v)] else
    if rarg = v then [IMulq (larg, v)] else
    [Movq (larg, v); IMulq (rarg, v)]
  | CBinOp ("/", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    [Movq (larg, Reg Rax); Cqto; IDivq (rarg); Movq (Reg Rax, v)]
  | CBinOp ("%", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    [Movq (larg, Reg Rax); Cqto; IDivq (rarg); Movq (Reg Rdx, v)]
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
    (* print_endline ("Alloc: " ^ (string_of_aarg v) ^ " : " ^ (string_of_int (8 * (i + 1)))); *)
    [
      Movq (GlobalValue free_ptr, v);
      Addq (AInt (8 * (i + 1)), GlobalValue free_ptr);
      Movq (v, Reg R11);
      Leaq (TypeRef dt, Reg Rcx);
      Movq (Reg Rcx, Deref (R11, 0))
    ]
  | CVectorRef (ve, i) ->
    let varg = get_aarg_of_carg ve in
    [Movq (varg, Reg R11); Movq (Deref (R11, 8 * (i + 1)), v)]

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
    ACallq ("collect", [Reg root_stack_register; AInt i; get_collect_call_count ()], AVoid) :: select_stmts t
  | CVectorSet (ve, i, ne) :: t ->
    let varg = get_aarg_of_carg ve in
    let earg = get_aarg_of_carg ne in
    Movq (varg, Reg R11) :: Movq (earg, Deref (R11, 8 * (i + 1))) :: select_stmts t
| [] -> []

let select_instructions program : pprogram =
  match program with
  | CProgram (vars, datatype, defs, stmts) ->
    PProgram (vars, datatype, select_stmts stmts)
