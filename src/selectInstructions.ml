open AProgram
open CProgram
open RProgram
open Registers
open Helper
open List

exception SelectInstructionError of string
let select_instruction_error s = raise (SelectInstructionError s)

(* Store how many times we call collect for debugging purposes *)
let collect_call_count = ref 0

let get_collect_call_count () =
  collect_call_count := !collect_call_count + 1;
  AInt !collect_call_count

let rec select_print_instrs dt arg =
  match dt with
  | TypeInt -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_int"]
  | TypeChar -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_char"]
  | TypeBool -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_bool"]
  | TypeVoid -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_void"]
  | TypeFunction (args, ret) -> [Leaq (TypeRef dt, Reg Rdi); Movq (AInt 1, Reg Rdx); Callq "print_function"]
  | TypeArray l -> [Movq (arg, Reg Rdi); Leaq (TypeRef dt, Reg Rsi); Movq (AInt 1, Reg Rdx); Callq "print_array"]
  | TypeVector l -> match l with
    | TypeFunction (args, ret) :: [] -> select_print_instrs (TypeFunction (args, ret)) arg
    | _ -> [Movq (arg, Reg Rdi); Leaq (TypeRef dt, Reg Rsi); Movq (AInt 1, Reg Rdx); Callq "print_vector"]

let select_exp e v : ainstr list =
  match e with
  | CArg (CFunctionRef label) ->
    [Leaq (GlobalValue label, v)]
  | CArg a ->
    let arg = get_aarg_of_carg a in
    [Movq (arg, v)]
  | CPrint (dt, a) ->
    let arg = get_aarg_of_carg a in
    select_print_instrs dt arg
  | CRead ->
    [ACallq (GlobalValue "read_int", [], v)]
  | CUnOp (o, a) ->
    let arg = get_aarg_of_carg a in
    if arg = v then [Negq v] else [Movq (arg, v); Negq v]
  | CBinOp ("-", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    if larg = v then [Subq (rarg, v)] else
    if rarg = v then [Subq (larg, v)] else
    [Movq (larg, v); Subq (rarg, v)]
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
  | CArrayRef (a, i) ->
    let varg = get_aarg_of_carg a in
    let iarg = get_aarg_of_carg i in
    (* Move array to R11 *)
    Movq (varg, Reg Rax) ::
    Movq (Reg Rax, Reg R11) ::
    (* Calculate array offset - i.e. index to update *)
    Movq (iarg, Reg Rcx) ::
    IMulq (AInt 8, Reg Rcx) ::
    (* 8 for tag, 8 for array-length that's prepended to elements *)
    Addq (AInt 16, Reg Rcx) ::
    (* Update array index with new value *)
    Movq (DerefVar (R11, Rcx), v) :: []
  | CApply (id, args) ->
    let aargs = map (fun a -> get_aarg_of_carg a) args in
    let nid = get_aarg_of_carg id in
    [ACallq (nid, aargs, v)]

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
    ACallq (GlobalValue "collect", [Reg root_stack_register; AInt i; get_collect_call_count ()], AVoid) :: select_stmts t
  (* Handles array-sets generated from expose-allocation where we have an actual int for i *)
  | CArraySet (ve, CInt i, ne) :: t ->
    let varg = get_aarg_of_carg ve in
    let earg = get_aarg_of_carg ne in
    (* Move array to R11 *)
    Movq (varg, Reg Rax) :: Movq (Reg Rax, Reg R11) ::
    (* Update array index with new value *)
    Movq (earg, Deref (R11, 8 + 8 + (8 * i))) :: select_stmts t
  (* Handles array-sets for indexs unknown at compile-time *)
  | CArraySet (ve, i, ne) :: t ->
    let varg = get_aarg_of_carg ve in
    let iarg = get_aarg_of_carg i in
    let earg = get_aarg_of_carg ne in
    (* Move array to R11 *)
    Movq (varg, Reg Rax) ::
    Movq (Reg Rax, Reg R11) ::
    (* Calculate array offset - i.e. index to update *)
    Movq (iarg, Reg Rcx) ::
    IMulq (AInt 8, Reg Rcx) ::
    Addq (AInt 16, Reg Rcx) ::
    (* Update array index with new value *)
    Movq (earg, DerefVar (R11, Rcx)) :: select_stmts t
  | CVectorSet (ve, i, ne) :: t ->
    let varg = get_aarg_of_carg ve in
    let earg = get_aarg_of_carg ne in
    Movq (varg, Reg R11) :: Movq (earg, Deref (R11, 8 * (i + 1))) :: select_stmts t
| [] -> []

let rec select_defs defs =
  match defs with
  | CDefine (id, args, ret, vars, stmts) :: t ->
    let num_params = length args in
    let var_types = Hashtbl.create 10 in
    let var_asc = remove_duplicates (tbl_to_list vars @ args) in
    List.iter (fun (id, dt) -> Hashtbl.replace var_types id dt) var_asc;
    let max_stack = 0 in  (* Calculate in assign-homes when doing callqs *)
    let movs = mapi (fun i (a, dt) -> Movq (nth arg_locations i, nth callee_save_aregisters i)) args in
    let map_movs = mapi (fun i (a, dt) -> Movq (nth callee_save_aregisters i, AVar a)) args in
    let instrs = movs @ map_movs @ select_stmts stmts in
    let new_args = map (fun (id, dt) -> AVar id) args in
    PDefine (id, num_params, new_args, var_types, max_stack, instrs) :: select_defs t
  | [] -> []

let select_instructions program : pprogram =
  match program with
  | CProgram (vars, datatype, defs, stmts) ->
    let new_defs = select_defs defs in
    let new_stmts = select_stmts stmts in
    let print_res =
      (* if datatype <> TypeVoid then  *)
    select_print_instrs datatype (Reg Rax)
    (* else []  *)
    in
    PProgram (vars, datatype, new_defs, new_stmts @ print_res)
