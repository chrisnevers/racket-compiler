open List
open Stream

(* token *)

type token =
  | TProgram
  | TInt of int
  | TBool of bool
  | TVar of string
  | TArithOp of string
  | TCmpOp of string
  | TLogOp of string
  | TRead
  | TLet
  | TIf
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TVector
  | TVectorSet
  | TVectorRef
  | TVoid
  | TEOF

let string_of_token t =
  match t with
  | TProgram -> "Program"
  | TInt i -> "Int " ^ (string_of_int i)
  | TBool b -> "Bool " ^ (string_of_bool b)
  | TVar v -> "Var " ^ v
  | TArithOp o -> "ArithOp " ^ o
  | TCmpOp o -> "CmpOp " ^ o
  | TLogOp o -> "LogOp " ^ o
  | TRead -> "Read"
  | TLet -> "Let"
  | TIf -> "If"
  | TLParen -> "("
  | TRParen -> ")"
  | TLBracket -> "["
  | TRBracket -> "]"
  | TVector -> "vector"
  | TVectorSet -> "vector-set!"
  | TVectorRef -> "vector-ref"
  | TVoid -> "void"
  | TEOF -> "EOF"

let print_tokens tokens =
  List.iter (fun t -> print_string ((string_of_token t) ^ " ")) tokens;
  print_string "\n";
(* rProgram *)

type datatype =
  | TypeInt
  | TypeBool
  | TypeVoid
  | TypeVector of datatype list

type rexp =
  | RVar of string
  | RInt of int
  | RBool of bool
  | RAnd of rexp * rexp
  | RNot of rexp
  | RIf of rexp * rexp * rexp
  | RCmp of string * rexp * rexp
  | RUnOp of string * rexp
  | RBinOp of string * rexp * rexp
  | RLet of string * rexp * rexp
  | RRead
  | RVector of rexp list
  | RVectorRef of rexp * int
  | RVectorSet of rexp * int * rexp
  | RVoid

type rprogram =
  | RProgram of datatype * rexp

let string_of_datatype dt : string =
  match dt with
  | TypeInt -> "int"
  | TypeBool -> "bool"
  | TypeVoid -> "void"

let rec string_of_rexp e : string =
  "(" ^ (fun e ->
  match e with
  | RVar v -> "Var " ^ v
  | RInt i -> "Int " ^ (string_of_int i)
  | RBool b -> "Bool " ^ (string_of_bool b)
  | RAnd (l, r) -> "And " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | RNot e -> "Not " ^ (string_of_rexp e)
  | RIf (cnd, thn, els) -> "If " ^ (string_of_rexp cnd) ^ " then " ^ (string_of_rexp thn) ^ " else " ^ (string_of_rexp els)
  | RCmp (o, l, r) -> o ^ " " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | RUnOp (o, e) -> o ^ " " ^ (string_of_rexp e)
  | RBinOp (o, l, r) -> o ^ " " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | RLet (v, i, b) -> "Let ([Var " ^ v ^ " " ^ (string_of_rexp i) ^ "]) " ^ (string_of_rexp b)
  | RRead -> "Read"
  ) e
  ^ ")"

let print_rprogram p =
  match p with
  | RProgram (dt, e) -> print_endline ("Program : " ^ (string_of_datatype dt) ^ " " ^ (string_of_rexp e))

(* cProgram *)


type ccmp =
  | CEq | CL | CLE | CG | CGE

type carg =
  | CInt of int
  | CVar of string
  | CBool of bool

type cexp =
  | CArg of carg
  | CRead
  | CUnOp of string * carg
  | CBinOp of string * carg * carg
  | CNot of carg
  | CCmp of ccmp * carg * carg

type cstmt =
  | CAssign of string * cexp
  | CReturn of carg
  | CIf of cexp * cstmt list * cstmt list

type cprogram =
  | CProgram of string list * datatype * cstmt list

let string_of_ccmp o : string =
  match o with
  | CEq -> "eq?"
  | CL -> "<"
  | CLE -> "<="
  | CG -> ">"
  | CGE -> ">="

let string_of_carg a : string =
  "(" ^ (fun e ->
  match a with
  | CInt i -> "Int " ^ (string_of_int i)
  | CVar v -> "Var " ^ v
  | CBool b -> "Bool " ^ (string_of_bool b)
  ) a
  ^ ")"

let string_of_cexp e : string =
  "(" ^ (fun e ->
  match e with
  | CArg a -> "Arg " ^ (string_of_carg a)
  | CRead -> "Read"
  | CUnOp (o, a) -> "UnOp " ^ o ^ " " ^ (string_of_carg a)
  | CBinOp (o, l, r) -> "BinOp " ^ o ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  | CNot a -> "Not " ^ (string_of_carg a)
  | CCmp (cmp, l, r) -> "Cmp " ^ (string_of_ccmp cmp) ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  ) e
  ^ ")"

let rec string_of_cstmts stmts : string =
  List.fold_left (fun acc s -> acc ^ string_of_cstmt s ^ "\n\t") "" stmts

and string_of_cstmt a : string =
  "(" ^ (fun e ->
  match a with
  | CAssign (v, e) -> "Assign " ^ v ^ " " ^ (string_of_cexp e)
  | CReturn a -> "Return " ^ (string_of_carg a)
  | CIf (cnd, thn, els) -> "If " ^ (string_of_cexp cnd) ^ "\n\t\t" ^ (string_of_cstmts thn) ^ "\t" ^ (string_of_cstmts els)
  ) a
  ^ ")"

let string_of_string_list l : string =
  List.fold_left (fun acc s -> acc ^ s ^ " ") "" l

let print_cprogram program =
  match program with
  | CProgram (vars, dt, stmts) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype dt) ^ 
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nStmts\t: \n\t[\n\t" ^ (string_of_cstmts stmts) ^ "]"
    )

(* aProgram *)


type aregister =
  | Rsp   | Rbp
  | Rax   | Rbx
  | Rcx   | Rdx
  | Rsi   | Rdi
  | R8    | R9
  | R10   | R11
  | R12   | R13
  | R14   | R15
  | Al

type acmp =
  | AE
  | AL
  | ALE
  | AG
  | AGE

type aarg =
  | AInt of int
  | AVar of string
  | Reg of aregister
  | Deref of aregister * int
  | ByteReg of aregister

type ainstr =
  | Addq of aarg * aarg
  | Subq of aarg * aarg
  | Movq of aarg * aarg
  | Retq
  | Negq of aarg
  | Callq of string
  | Pushq of aarg
  | Popq of aarg
  | Xorq of aarg * aarg
  | Cmpq of aarg * aarg
  | Set of acmp * aarg
  | Movzbq of aarg * aarg
  | Jmp of string
  | JmpIf of acmp * string
  | Label of string
  | AIf of (acmp * aarg * aarg) * ainstr list * aarg list list * ainstr list * aarg list list

type aprogram =
  AProgram of int * datatype * ainstr list

type pprogram =
  PProgram of string list * datatype * ainstr list

type lprogram =
  LProgram of string list * aarg list list * datatype * ainstr list

type interference = ((aarg, aarg list) Hashtbl.t)

type gprogram =
  GProgram of string list * interference * datatype * ainstr list

let get_aarg_of_carg c : aarg =
  match c with
  | CVar v -> AVar v
  | CInt i -> AInt i
  | CBool true -> AInt 1
  | CBool false -> AInt 0

let get_acmp_of_ccmp c : acmp =
  match c with
  | CEq -> AE
  | CL -> AL
  | CLE -> ALE
  | CG -> AG
  | CGE -> AGE

let string_of_acmp c : string =
  match c with
  | AE -> "eq?"
  | AL -> "<"
  | ALE -> "<="
  | AG -> ">"
  | AGE -> ">="

let string_of_register r : string =
  match r with
  | Rsp -> "rsp"
  | Rbp -> "rbp"
  | Rax -> "rax"
  | Rbx -> "rbx"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
  | Al -> "al"

let string_of_aarg a : string =
  "(" ^ (fun e ->
  match a with
  | AInt i -> "Int " ^ (string_of_int i)
  | AVar s -> "Var " ^ s
  | Reg r -> "Reg " ^ (string_of_register r)
  | Deref (r, i) -> "Deref " ^ (string_of_register r) ^ " " ^ (string_of_int i)
  | ByteReg r -> "ByteReg " ^ (string_of_register r)
  ) a
  ^ ")"

let string_of_aarg_list a : string =
  "[" ^
  List.fold_left (fun acc e -> acc ^ string_of_aarg e ^ " ") " " a
  ^ "]"

let rec string_of_ainstrs i : string =
  (List.fold_left (fun acc s -> acc ^ string_of_ainstr s ^ "\n\t") "" i)

and string_of_ainstr a : string =
  match a with
  | Addq (l, r) -> "Addq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Subq (l, r) -> "Subq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Movq (l, r) -> "Movq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Retq -> "Retq"
  | Negq e -> "Negq " ^ (string_of_aarg e)
  | Callq s -> "Callq " ^ s
  | Pushq e -> "Pushq " ^ (string_of_aarg e)
  | Popq e -> "Popq " ^ (string_of_aarg e)
  | Xorq (l, r) -> "Xorq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Cmpq (l, r) -> "Cmpq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Set (cmp, e) -> "JmpIf " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg e)
  | Movzbq (l, r) -> "Movzbq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Jmp s -> "Jmp " ^ s
  | JmpIf (cmp, s) -> "JmpIf " ^ (string_of_acmp cmp) ^ " " ^ s
  | Label s -> "Label " ^ s
  | AIf ((cmp, l, r), thn, thn_live_afters, els, els_live_afters) ->
    "If " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg l) ^ " " ^ (string_of_aarg r) ^
    "\n\t[\n\t" ^ (string_of_ainstrs thn) ^ "]\nThen Live:\t[" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" thn_live_afters) ^ "]\n[\n\t"
    ^ (string_of_ainstrs els) ^ "]\nElse Live:\t[" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" els_live_afters) ^ "]"

let print_pprogram p =
  match p with
  | PProgram (vars, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_aprogram p =
  match p with
  | AProgram (vars_space, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nSpace\t: " ^ (string_of_int vars_space) ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_lprogram p =
  match p with
  | LProgram (vars, live_afters, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nLive-Afters: [");
      List.iter (fun e -> print_endline ("\t" ^ string_of_aarg_list e)) live_afters;
      print_endline ("\t]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_gprogram p =
  match p with
  | GProgram (vars, graph, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nGraph\t: [");
      Hashtbl.iter (fun k v ->
        print_string ("\n\tNode\t: " ^ (string_of_aarg k) ^ "\n\tEdges\t: [");
        List.iter (fun e -> print_string ((string_of_aarg e) ^ ", ")) v;
        print_endline " ]";
      ) graph;
      print_endline ("\t]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_color_graph colors =
  print_endline "\nColor Graph:";
  Hashtbl.iter (fun k v ->
      print_endline ((string_of_aarg k) ^ " : " ^ (string_of_int v));
    ) colors;
  print_endline ""

let print_move_bias_graph tbl =
  print_endline "\nMove Bias Graph:";
  Hashtbl.iter (fun k v ->
      print_endline ((string_of_aarg k) ^ " : " ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg e)) "" v);
    )) tbl;
  print_endline ""

let callee_save_registers = ["rbx"; "r12"; "r13"; "r14"; "r15"]
let caller_save_registers = ["rax"; "rdx"; "rcx"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"]
let callee_save_aregisters = [Reg Rbx; Reg R12; Reg R13; Reg R14; Reg R15]
let caller_save_aregisters = [Reg Rax; Reg Rdx; Reg Rcx; Reg Rsi; Reg Rdi; Reg R8; Reg R9; Reg R10; Reg R11]
let os_label_prefix = "_"
let callee_save_stack_size = (List.length callee_save_registers) * 8

exception RegisterException of string

let register_of_string s : aarg =
    match s with
    | "rsp" -> Reg Rsp
    | "rbp" -> Reg Rbp
    | "rax" -> Reg Rax
    | "rbx" -> Reg Rbx
    | "rcx" -> Reg Rcx
    | "rdx" -> Reg Rdx
    | "rsi" -> Reg Rsi
    | "rdi" -> Reg Rdi
    | "r8" -> Reg R8
    | "r9" -> Reg R9
    | "r10" -> Reg R10
    | "r11" -> Reg R11
    | "r12" -> Reg R12
    | "r13" -> Reg R13
    | "r14" -> Reg R14
    | "r15" -> Reg R15
    | "al" -> Reg Al
    | _ -> raise (RegisterException "register does not exist")

(* helper *)


let cdr = fun (_, b) -> b
let car = fun (a, _) -> a

let print_adjacent_aargs adjacents =
  print_endline ("[" ^ List.fold_left (fun acc e -> acc ^ string_of_aarg e ^ ",") "" adjacents ^ "]")

let print_adjacent_colors colors =
  print_endline ("[" ^ List.fold_left (fun acc e -> acc ^ string_of_int e ^ ",") "" colors ^ "]")
(* registers *)


let registers = [Rbx; Rcx; Rdx; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14; R15]
let num_of_registers = List.length registers

(* lexer *)


exception LexerError of string

let lexer_error s = raise (LexerError s)

(* is character a digit? *)
let is_digit c : bool =
  let code = Char.code c in
  code >= Char.code('0') && code <= Char.code('9')

(* is character alphanumerical? *)
let is_alpha c : bool =
  let code = Char.code c in
  (code >= Char.code('A') && code <= Char.code('Z')) ||
  (code >= Char.code('a') && code <= Char.code('z'))

(* Construct a stream from a file or a string. Valid types are File or String *)
let get_stream src stream_type : char Stream.t =
  match stream_type with
  | `File ->
    let channel = open_in src in
    let stream = Stream.of_channel channel in stream
    (* let _ = close_in channel in stream *)
  | `String -> Stream.of_string src

(* Skips white space *)
let rec next_char stream : char =
  let nc = Stream.next stream in
  match nc with
  | ' ' | '\t' | '\n' -> next_char stream
  | c -> c

let peek_char stream : char option = Stream.peek stream

let is_valid_id c : bool =
  is_alpha c || is_digit c || c = '_' || c = '?' || c = '-' || c = '!'

let is_stream_empty stream : bool =
  try Stream.empty stream; true
  with Stream.Failure -> false

let rec scan_literal stream acc : token =
  let next = peek_char stream in
  match next with
  | Some c when is_digit c ->
    let _ = next_char stream in
    scan_literal stream (acc ^ (Char.escaped c))
  | _ -> TInt (int_of_string acc)

let rec scan_identifier stream acc : token =
  let next = peek_char stream in
  match next with
  | Some c when is_valid_id c ->
    let _ = next_char stream in
    scan_identifier stream (acc ^ (Char.escaped c))
  | _ ->
    match acc with
    | "program" -> TProgram
    | "read"    -> TRead
    | "let"     -> TLet
    | "if"      -> TIf
    | "and"     -> TLogOp "and"
    | "not"     -> TLogOp "not"
    | "eq?"     -> TCmpOp "eq?"
    | "void"    -> TVoid
    | "vector"  -> TVector
    | "vector-set!" -> TVectorSet
    | "vector-ref"  -> TVectorRef
    | _         -> TVar acc

let get_cmp_op c : token =
  match c with
  | 't' -> TBool true
  | 'f' -> TBool false
  | _ -> lexer_error ("scan_token: Expected #t or #f but received #" ^ (Char.escaped c))

let scan_token stream : token = try
  match is_stream_empty stream with
  | true -> TEOF
  | false ->
    let c = next_char stream in
    if is_alpha c then scan_identifier stream (Char.escaped c)
    else if is_digit c then scan_literal stream (Char.escaped c)
    else match c with
    | '+' -> TArithOp "+"
    | '-' -> TArithOp "-"
    | '(' -> TLParen
    | ')' -> TRParen
    | '[' -> TLBracket
    | ']' -> TRBracket
    | '>' ->
      let next = peek_char stream in
      if next = Some '=' then let _ = next_char stream in TCmpOp ">=" else TCmpOp ">"
    | '<' ->
      let next = peek_char stream in
      if next = Some '=' then let _ = next_char stream in TCmpOp "<=" else TCmpOp "<"
    | '#' ->
      let next = next_char stream in
      get_cmp_op next
    | _ -> lexer_error ("scan_token: Unrecognised token: " ^ (Char.escaped c))
  with Stream.Failure -> TEOF

let rec scan_all_tokens stream tokens : token list =
  let token = scan_token stream in
  if token = TEOF then tokens @ [token]
  else scan_all_tokens stream (tokens @ [token])

(* parser *)


exception ParserError of string

let parser_error s = raise (ParserError s)

let next_token tokens = hd !tokens

let get_token tokens =
  let token = next_token tokens in
  (tokens := tl !tokens; token)

let expect_token tokens expected =
  let actual = get_token tokens in
  if actual = expected then ()
  else parser_error ("Expected " ^ (string_of_token expected) ^ " but received " ^ (string_of_token actual))

let parse_var tokens =
  let actual = get_token tokens in
  match actual with
  | TVar v -> v
  | _ -> parser_error ("Expected var but received " ^ (string_of_token actual))

let parse_int tokens : int =
  let token = get_token tokens in
  match token with
  | TInt i -> i
  | _ -> parser_error ("Expected integer but received " ^ (string_of_token token))

let rec parse_exp tokens : rexp =
  let token = get_token tokens in
  match token with
  | TLParen ->
    let exp = parse_exp tokens in
    expect_token tokens TRParen; exp
  | TVoid -> RVoid
  | TInt i -> RInt i
  | TVar v -> RVar v
  | TBool b -> RBool b
  | TVector ->
    let exps = parse_inner_exps tokens [] in
    RVector exps
  | TVectorRef ->
    let exp = parse_exp tokens in
    let index = parse_int tokens in
    RVectorRef(exp, index)
  | TVectorSet ->
    let e1 = parse_exp tokens in
    let index = parse_int tokens in
    let e2 = parse_exp tokens in
    RVectorSet(e1, index, e2)
  | TRead -> RRead
  | TArithOp o ->
    let exp = parse_exp tokens in
    (match next_token tokens with
    | TRParen -> RUnOp (o, exp)
    | _ ->
      let exp2 = parse_exp tokens in
      RBinOp(o, exp, exp2))
  | TLogOp "and" ->
    let l = parse_exp tokens in
    let r = parse_exp tokens in
    RAnd (l, r)
  | TLogOp "not" ->
    let exp = parse_exp tokens in RNot exp
  | TCmpOp o ->
    let l = parse_exp tokens in
    let r = parse_exp tokens in
    RCmp (o, l, r)
  | TLet ->
    expect_token tokens TLParen;
    expect_token tokens TLBracket;
    let v = parse_var tokens in
    let i = parse_exp tokens in
    expect_token tokens TRBracket;
    expect_token tokens TRParen;
    let b = parse_exp tokens in
    RLet (v, i, b)
  | TIf ->
    let cnd = parse_exp tokens in
    let thn = parse_exp tokens in
    let els = parse_exp tokens in
    RIf (cnd, thn, els)
  | _ -> parser_error ("Did not expect token " ^ (string_of_token token))

and parse_inner_exps tokens exps =
  let next = next_token tokens in
  match next with
  | TRParen -> exps
  | _ -> parse_inner_exps tokens (parse_exp tokens :: exps)

let parse_program tokens : rprogram =
  expect_token tokens TLParen;
  expect_token tokens TProgram;
  let exp = parse_exp tokens in
  expect_token tokens TRParen;
  expect_token tokens TEOF;
  RProgram (TypeVoid, exp)

let parse tokens =
  let token_list = ref tokens in
  parse_program token_list

(* uniquify *)


exception UniquifyError of string

let uniquify_error s = raise (UniquifyError s)

let get_var_name v table : string =
  try
    let count = Hashtbl.find table v in
    match count with
    | 1 -> v
    | _ -> v ^ (string_of_int count)
  with Not_found -> uniquify_error ("get_var_name: Variable " ^ v ^ " is undefined")

let uniquify_name v table : string =
  try
    let count = (Hashtbl.find table v) + 1 in
    let _ = Hashtbl.replace table v count in v ^ (string_of_int count)
  with Not_found -> 
    let _ = Hashtbl.add table v 1 in v

let rec uniquify_exp ast table : rexp =
  match ast with
  | RLet (v, i, b) ->
    let iexp = uniquify_exp i table in
    let uniq_var = uniquify_name v table in
    let bexp = uniquify_exp b table in
    RLet (uniq_var, iexp, bexp)
  | RUnOp (o, e) -> RUnOp (o, uniquify_exp e table)
  | RBinOp (o, l, r) -> RBinOp (o, uniquify_exp l table, uniquify_exp r table)
  | RVar v -> RVar (get_var_name v table)
  | RAnd (l, r) -> RAnd (uniquify_exp l table, uniquify_exp r table)
  | RNot e -> RNot (uniquify_exp e table)
  | RIf (cnd, thn, els) -> RIf (uniquify_exp cnd table, uniquify_exp thn table, uniquify_exp els table)
  | RCmp (o, l, r) -> RCmp (o, uniquify_exp l table, uniquify_exp r table)
  | _ -> ast

let uniquify ast : rprogram =
  match ast with
  | RProgram (dt, e) -> RProgram (dt, uniquify_exp e (Hashtbl.create 10))
  
(* typecheck *)


exception TypecheckError of string

let typecheck_error s = raise (TypecheckError s)

let get_var_type v table =
  try Hashtbl.find table v
  with Not_found -> typecheck_error "get_var_type: Undeclared variable"

let rec typecheck_exp exp table : datatype =
  match exp with
  | RInt i -> TypeInt
  | RBool b -> TypeBool
  | RVar v -> get_var_type v table
  | RAnd (l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    if ltype = TypeBool && rtype = TypeBool then TypeBool
    else typecheck_error "typecheck_exp: And expressions must operate on boolean values"
  | RNot e ->
    let etype = typecheck_exp e table in
    if etype = TypeBool then TypeBool
    else typecheck_error "typecheck_exp: Not expressions must operate on boolean values"
  | RIf (cnd, thn, els) ->
    let ctype = typecheck_exp cnd table in
    let ttype = typecheck_exp thn table in
    let etype = typecheck_exp els table in
    if ctype != TypeBool then typecheck_error "typecheck_exp: If condition must evaluate to boolean value"
    else if ttype = etype then etype
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same type"
  | RCmp (o, l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    (match o with
    | ">" | ">=" | "<" | "<=" -> 
      if ltype = TypeInt && rtype = TypeInt then TypeBool
      else typecheck_error ("typecheck_exp: " ^ o ^ " operates on integers")
    | "eq?" -> 
      if ltype = rtype then TypeBool
      else typecheck_error "typecheck_exp: eq? only compares same type"
    | _ -> typecheck_error "typecheck_exp: unexpected compare operator")
  | RUnOp (o, e) ->
    let etype = typecheck_exp e table in
    if etype = TypeInt then TypeInt
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integer")
  | RBinOp (o, l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    if ltype = TypeInt && rtype = TypeInt then TypeInt
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integers")
  | RLet (v, i, b) ->
    let itype = typecheck_exp i table in
    let _ = Hashtbl.add table v itype in
    let btype = typecheck_exp b table in
    btype
  | RRead -> TypeInt

let typecheck program : rprogram =
  match program with
  | RProgram (_, e) -> RProgram (typecheck_exp e (Hashtbl.create 10), e)

(* flatten *)


exception FlattenError of string

let flatten_error s = raise (FlattenError s)

let get_var_name v tmp_count =
  match v with
  | Some name -> name
  | None -> 
    tmp_count := !tmp_count + 1;
    "tmp" ^ (string_of_int !tmp_count)

let get_carg_of_rarg a : carg =
  match a with 
  | RBool b -> CBool b
  | RInt i -> CInt i
  | RVar name -> CVar name
  | _ -> flatten_error ("get_carg_of_rarg: Expected to receive CArg but received " ^ (string_of_rexp a))

let get_ccmp_of_rcmp o : ccmp =
  match o with
  | "eq?" -> CEq
  | "<" -> CL
  | "<=" -> CLE
  | ">" -> CG
  | ">=" -> CGE
  | _ -> flatten_error ("get_ccmp_of_rcmp: Expected compare operator but received " ^ o)

let flatten_arg ?(v=None) a tmp_count : carg * cstmt list * string list =
  let flat_arg = get_carg_of_rarg a in
  let stmts = if v = None then [] else [CAssign (get_var_name v tmp_count, CArg flat_arg)] in
  let var_list = [] in
  (flat_arg, stmts, var_list)

let rec flatten_exp ?(v=None) e tmp_count : carg * cstmt list * string list =
  match e with
  | RVar _ | RInt _ | RBool _ -> 
    flatten_arg e tmp_count ~v:v
  | RAnd (l, r) ->
    let (larg, lstmts, lvars) = flatten_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let lif_cnd = CCmp (CEq, CBool true, larg) in
    let rif_cnd = CCmp (CEq, CBool true, rarg) in
    (* We only execute this if first condition is true, so if this condition is true, then set var to true, otherwise false *)
    let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
    (* Execute first condition, if true continue to next condition (execute its stmts then call if to see if true), otherwise short circuit to false *)
    let lif = CIf (lif_cnd, rstmts @ [rif], [CAssign (var_name, CArg (CBool false))]) in
    (* Execute lstmts see if left is true *)
    let stmts = lstmts @ [lif] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)
  | RNot e ->
    let (earg, estmts, evars) = flatten_exp e tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = estmts @ [CAssign (var_name, CNot earg)] in
    let var_list = if v = None then var_name :: evars else evars in
    (flat_arg, stmts, var_list)    
  | RIf (cnd, thn, els) ->
    let var_name = get_var_name v tmp_count in
    let (cnd_arg, cnd_stmts, cnd_vars) = flatten_exp cnd tmp_count in
    (* Assign result of then and else conditions to lhs variable / or newly created tmp *)
    let (thn_arg, thn_stmts, thn_vars) = flatten_exp thn tmp_count ~v:(Some var_name) in
    let (els_arg, els_stmts, els_vars) = flatten_exp els tmp_count ~v:(Some var_name) in
    let if_cnd = CCmp (CEq, CBool true, cnd_arg) in
    let flat_arg = CVar var_name in
    let stmts = cnd_stmts @ [CIf (if_cnd, thn_stmts, els_stmts)] in
    let var_list = if v = None then var_name :: cnd_vars @ thn_vars @ els_vars else cnd_vars @ thn_vars @ els_vars in
    (flat_arg, stmts, var_list)   
  | RCmp (o, l, r) ->
    let (larg, lstmts, lvars) = flatten_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let ccmp = get_ccmp_of_rcmp o in
    let stmts = lstmts @ rstmts @ [CAssign (var_name, CCmp (ccmp, larg, rarg))] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)   
  | RUnOp (o, e) ->
    let (earg, estmts, evars) = flatten_exp e tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = estmts @ [CAssign (var_name, CUnOp (o, earg))] in
    let var_list = if v = None then var_name :: evars else evars in
    (flat_arg, stmts, var_list)
  | RBinOp (o, l, r) ->
    let (larg, lstmts, lvars) = flatten_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = lstmts @ rstmts @ [CAssign (var_name, CBinOp (o, larg, rarg))] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)
  | RLet (name, i, b) ->
    (* Assign result of inner expression to the variable being declared *)
    let (iarg, istmts, ivars) = flatten_exp i tmp_count ~v:(Some name) in
    (* Assign result of body function to whatever variable this expression is a child of *)
    let (barg, bstmts, bvars) = flatten_exp b tmp_count ~v:v in
    let flat_arg = barg in
    let stmts = istmts @ bstmts in
    let var_list = name :: ivars @ bvars in
    (flat_arg, stmts, var_list)
  | RRead ->
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = [CAssign (var_name, CRead)] in
    let var_list = if v = None then [var_name] else [] in
    (flat_arg, stmts, var_list)

let flatten program : cprogram =
  match program with
  | RProgram (dt, e) ->
    let tmp_count = ref 0 in
    let (arg, stmts, vars) = flatten_exp e tmp_count in
    let new_stmts = stmts @ [CReturn arg] in
    CProgram (vars, dt, new_stmts)

(* selectInstructions *)


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

(* uncoverLive *)


let get_var_list_or_empty v : aarg list =
  match v with
  | AVar _ -> [v]
  | _ -> []

let get_written_vars i =
  match i with
  | Movq (l, r) | Addq (l, r) | Subq (l, r)
  | Movzbq (l, r) | Xorq (l, r) -> get_var_list_or_empty r
  | Set (l, r) -> get_var_list_or_empty r
  | Negq e -> get_var_list_or_empty e
  | _ -> []

let get_read_vars i =
  match i with
  | Addq (l, r) | Subq (l, r) | Cmpq (l, r) | Xorq (l, r) -> get_var_list_or_empty l @ get_var_list_or_empty r
  | Movq (l, r) | Movzbq (l, r) -> get_var_list_or_empty l
  | Negq e -> get_var_list_or_empty e
  | _ -> []

let rec uncover stmts live_after : (ainstr * aarg list) list =
  match stmts with
  | AIf ((o, l, r), thn, _, els, _) :: t ->
    let (thn_stmts, thn_live_after) = List.split (List.rev (uncover (List.rev thn) live_after)) in
    let (els_stmts, els_live_after) = List.split (List.rev (uncover (List.rev els) live_after)) in
    let live_now = List.sort_uniq compare (List.concat(thn_live_after) @ List.concat(els_live_after) @ get_var_list_or_empty l @ get_var_list_or_empty r) in
    (AIf ((o, l, r), thn_stmts, thn_live_after, els_stmts, els_live_after), live_now) :: uncover t live_now
  | s :: t ->
    let written = get_written_vars s in
    let read = get_read_vars s in
    let live_now = List.sort_uniq compare ((List.filter (fun e -> not (List.mem e written)) live_after) @ read) in
    (s, live_now) :: (uncover t live_now)
  | [] -> []

let uncover_live program : lprogram =
  match program with
  | PProgram (vars, datatype, stmts) ->
    let (new_stmts, live_afters) = List.split (List.rev (uncover (List.rev stmts) [])) in
    LProgram (vars, live_afters, datatype, new_stmts)

(* buildInterference *)


let find_in_map key map : aarg list =
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map : unit =
  let current_value = find_in_map key map in
  Hashtbl.remove map key;
  Hashtbl.add map key (List.sort_uniq compare (value :: current_value))

let add_bidirected_edge n1 n2 map : unit =
  append_to_value n1 n2 map;
  append_to_value n2 n1 map

let rec add_edges cnd (d: aarg) (targets: aarg list) map =
  match targets with
  | n :: t ->
    if cnd n then (add_bidirected_edge d n map; add_edges cnd d t map)
    else add_edges cnd d t map
  | [] -> ()

let rec add_edges_from_nodes nodes targets map =
  match nodes with
  | h :: t ->
    add_edges (fun v -> true) h targets map;
    add_edges_from_nodes t targets map
  | [] -> ()

let rec build_graph stmts live_afters map : interference =
  match stmts with
  | Movq (s, d) :: t | Movzbq(s, d) :: t ->
    let live_vars = hd (live_afters) in
    (* add the edge (d, v) for every v of Lafter(k) unless v = d or v = s. *)
    add_edges (fun v -> v <> d && v <> s) d live_vars map;
    build_graph t (tl live_afters) map
  | Addq (s, d) :: t | Subq (s, d) :: t (* | XOrq :: t ? *)->
    let live_vars = hd (live_afters) in
    (* add the edge (d, v) for every v of Lafter(k) unless v = d. *)
    add_edges (fun v -> v <> d) d live_vars map;
    build_graph t (tl live_afters) map

  (* TODO: Ask Jay
  | Callq label :: t ->
    let live_vars = hd (live_afters) in
    add an edge (r, v) for every caller-save register r and every variable v of Lafter(k).
    add_edges_from_nodes caller_save_aregisters live_vars map;
    build_graph t (tl live_afters) map *)

  | AIf ((c, s, d), thn_instrs, thn_lafter, els_instrs, els_lafter) :: t ->
    let _ = build_graph thn_instrs thn_lafter map in
    let _ = build_graph els_instrs els_lafter map in
    build_graph t (tl live_afters) map
  | h :: t -> build_graph t (tl live_afters) map
  | [] -> map

let build_interference program : gprogram =
  match program with
  | LProgram (vars, live_afters, datatype, stmts) ->
    let map = build_graph stmts live_afters (Hashtbl.create 10) in
    GProgram (vars, map, datatype, stmts)

(* allocateRegisters *)


let is_var a = match a with AVar _ -> true | _ -> false

let find_in_map key map =
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map =
  let current_value = find_in_map key map in
  Hashtbl.replace map key (List.sort_uniq compare (value :: current_value))

let create_graph keys value =
  let graph = Hashtbl.create 10 in
  let rec add_to_table ks =
    match ks with
    | key :: tail -> Hashtbl.replace graph (AVar key) value; add_to_table tail
    | [] -> ()
  in
  let _ = add_to_table keys in graph

let rec get_most_saturated graph saturations =
  let current = ref (AVar "", 0) in
  Hashtbl.iter (fun k v ->
    let no_of_saturations = List.length (find_in_map k saturations) in
    if no_of_saturations >= (cdr !current) && (is_var k) then
      current := (k, no_of_saturations)) graph;
  (car !current)

let rec get_lowest_color adjacent_colors cur =
  match adjacent_colors with
  | h :: t ->
    if h != -1 && h = cur then
      get_lowest_color t (cur + 1)
    else
      get_lowest_color t cur
  | [] -> cur

let rec add_color_to_saturations saturations adjacents color =
  match adjacents with
  | h :: t ->
    append_to_value h color saturations;
    add_color_to_saturations saturations t color
  | [] -> ()

let get_adjacent_colors colors adjacents =
  List.sort compare (
    List.map (fun e -> Hashtbl.find colors e) adjacents
  )
  (* match adjacents with
  | h :: t -> Hashtbl.find colors h :: (get_adjacent_colors colors t)
  | [] -> [] *)

let rec get_colors graph saturations colors move =
  match Hashtbl.length graph with
  | 0 -> colors
  | _ ->
    (* Pick node in graph with highest saturation *)
    let max_saturated = get_most_saturated graph saturations in
    (* Find its neighboring nodes *)
    let adjacents = Hashtbl.find graph max_saturated in
    (* Find what its neighboring nodes are already assigned *)
    let adjacent_colors = List.sort compare (get_adjacent_colors colors adjacents) in
    (* Find its move bias neighboring nodes *)
    let move_adjacents = find_in_map max_saturated move in
    (* Find whats its move bias neighbors are already assigned *)
    let bias_colors = List.filter (fun e -> e != (-1) && not (List.mem e adjacent_colors))
                      (List.sort compare (get_adjacent_colors colors move_adjacents)) in
    (* Pick lowest number not in neighboring nodes *)
    let lowest_color = if bias_colors = [] then get_lowest_color adjacent_colors 0 else hd bias_colors in
    (* Add chosen color to final color map *)
    Hashtbl.replace colors max_saturated lowest_color;
    add_color_to_saturations saturations adjacents lowest_color;
    (* Remove node from processing list *)
    Hashtbl.remove graph max_saturated;
    get_colors graph saturations colors move

let color_graph graph vars move =
  (* List of numbers a variable cannot be assigned *)
  let saturations = create_graph vars [] in
  (* The color (number) a variable is assigned *)
  let colors = create_graph vars (-1) in
  get_colors (Hashtbl.copy graph) saturations colors move

(* Map the variable to a register or spill to the stack if no space *)
let get_register a graph =
  match a with
  | AVar v ->
    let index = Hashtbl.find graph a in
    if index >= num_of_registers then a
    else if index = -1 then Reg Rbx
    else Reg (List.nth registers index)
  | _ -> a

let rec get_new_instrs instrs graph =
  match instrs with
  | [] -> []
  | Addq (a, b) :: tl ->
    Addq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Subq (a, b) :: tl ->
    Subq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Movq (a, b) :: tl ->
    let a_register = get_register a graph in
    let b_register = get_register b graph in
    if (a_register = b_register) then get_new_instrs tl graph
    else Movq (a_register, b_register) :: get_new_instrs tl graph
  | Xorq (a, b) :: tl ->
    Xorq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Cmpq (a, b) :: tl ->
    Cmpq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Movzbq (a, b) :: tl ->
    Movzbq (get_register a graph, get_register b graph) :: get_new_instrs tl graph
  | Set (c, b) :: tl ->
    Set (c, get_register b graph) :: get_new_instrs tl graph
  | Jmp l :: tl ->
    Jmp l :: get_new_instrs tl graph
  | JmpIf (c, l) :: tl ->
    JmpIf (c, l) :: get_new_instrs tl graph
  | Label l :: tl ->
    Label l :: get_new_instrs tl graph
  | Negq a :: tl ->
    Negq (get_register a graph) :: get_new_instrs tl graph
  | Callq l :: tl ->
    Callq l :: get_new_instrs tl graph
  | Pushq a :: tl ->
    Pushq (get_register a graph) :: get_new_instrs tl graph
  | Popq a :: tl ->
    Popq (get_register a graph) :: get_new_instrs tl graph
  | Retq :: tl ->
    Retq :: get_new_instrs tl graph
  | AIf ((c, a, b), thn_instr, _, els_instr, _):: tl ->
    AIf ( (c, get_register a graph, get_register b graph),
          get_new_instrs thn_instr graph, [],
          get_new_instrs els_instr graph, []) :: get_new_instrs tl graph

let rec create_move_bias_graph instrs tbl =
  match instrs with
  | Movq (s, d) :: tl when is_var s && is_var d ->
    append_to_value s d tbl;
    append_to_value d s tbl;
    create_move_bias_graph tl tbl
  | _ :: tl -> create_move_bias_graph tl tbl
  | [] -> tbl

let allocate_registers program : gprogram =
  match program with
  | GProgram (vars, graph, datatype, instrs) ->
    (* Create move bias graph to doc which vars are movq'd to other vars *)
    let move = create_move_bias_graph instrs (Hashtbl.create 10) in
    (* Assign each var a color unique to its adjacent nodes *)
    let colors = color_graph graph vars move in
    print_color_graph colors;
    (* Reiterate over instructions & replace vars with registers *)
    let new_instrs = get_new_instrs instrs colors in
    GProgram (vars, graph, datatype, new_instrs)

(* lowerConditionals *)


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
    Cmpq (a2, a1) :: JmpIf (c, thn_label) :: lower_instructions els_instrs uniq_cnt @
    Jmp end_label :: Label thn_label :: lower_instructions thn_instrs uniq_cnt @
    Label end_label :: lower_instructions tl uniq_cnt
  | h :: tl -> h :: lower_instructions tl uniq_cnt

let lower_conditionals program =
  match program with
  | GProgram (vars, interference, datatype, instrs) ->
    let uniq_count = ref 0 in
    let new_instrs = lower_instructions instrs uniq_count in
    GProgram (vars, interference, datatype, new_instrs)

(* assignHomes *)


let make_multiple_of_16 i =
  let remainder = i mod 16 in
  if remainder = 0 then i
  else i + (16 - remainder)

let get_register_offset arg homes offset =
  try
    Hashtbl.find homes arg
  with
  | Not_found ->
    offset := !offset - 8;
    Hashtbl.replace homes arg !offset;
    !offset

let get_arg_home arg homes offset =
  match arg with
  | AVar v -> Deref (Rbp, get_register_offset arg homes offset)
  | _ -> arg

let rec get_instrs instrs homes offset =
  match instrs with
  | [] -> []
  | Addq (a, b) :: tail ->
    Addq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Subq (a, b) :: tail ->
    Subq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Movq (a, b) :: tail ->
    Movq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Negq a :: tail ->
    Negq (get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Callq l :: tail ->
    Callq l :: (get_instrs tail homes offset)
  | Pushq a :: tail ->
    Pushq (get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Popq a :: tail ->
    Popq (get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Xorq (a, b) :: tail ->
    Xorq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Cmpq (a, b) :: tail ->
    Cmpq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Movzbq (a, b) :: tail ->
    Movzbq (get_arg_home a homes offset, get_arg_home b homes offset) :: (get_instrs tail homes offset)
  | Set (c, a) :: tail ->
    Set (c, get_arg_home a homes offset) :: (get_instrs tail homes offset)
  | Retq :: tail ->
    Retq :: (get_instrs tail homes offset)
  | Jmp l :: tail ->
    Jmp l:: (get_instrs tail homes offset)
  | JmpIf (c, l) :: tail ->
    JmpIf (c, l):: (get_instrs tail homes offset)
  | Label l :: tail ->
    Label l :: (get_instrs tail homes offset)
  | AIf ((c, a, b), thn_instrs, _, els_instrs, _) :: tail ->
    AIf (
      (c, get_arg_home a homes offset, get_arg_home b homes offset),
      (get_instrs thn_instrs homes offset), [],
      (get_instrs els_instrs homes offset), []) :: (get_instrs tail homes offset)

let assign_homes program =
  match program with
  | GProgram (vars, graph, datatype, instrs) ->
    let homes = Hashtbl.create 10 in
    let offset = ref 0 in
    let new_instrs = get_instrs instrs homes offset in
    let var_space = make_multiple_of_16 (- !offset) in
    AProgram (var_space, datatype, new_instrs)

(* patchInstructions *)


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

(* printx86 *)


exception InvalidInstructionException of string

let invalid_instruction msg = raise (InvalidInstructionException msg)

let rec add_callee_save_registers registers op =
  match registers with
  | reg :: t -> "\t" ^ op ^ "\t%" ^ reg ^ "\n" ^ (add_callee_save_registers t op)
  | [] -> ""

let arg_to_x86 arg =
  match arg with
  | AInt i -> "$" ^ (string_of_int i)
  | Reg r | ByteReg r ->
    "%" ^ string_of_register r
  | Deref (r, i) ->
    (string_of_int i) ^ "(%" ^ string_of_register r ^ ")"
  | AVar v -> invalid_instruction ("Cannot print vars: " ^ v)

let cmp_to_x86 cmp =
  match cmp with
  | AE -> "e"
  | AL -> "l"
  | ALE -> "le"
  | AG -> "g"
  | AGE -> "ge"

let rec print_instrs instrs =
  match instrs with
  | [] -> ""
  | Addq (a, b) :: tl -> "\taddq\t" ^ arg_to_x86 a ^ ",\t" ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Subq (a, b) :: tl -> "\tsubq\t" ^ arg_to_x86 a ^ ",\t" ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Movq (a, b) :: tl -> "\tmovq\t" ^ arg_to_x86 a ^ ",\t" ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Negq a :: tl -> "\tnegq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Callq a :: tl -> "\tcallq\t" ^ os_label_prefix ^ a ^ "\n" ^ (print_instrs tl)
  | Pushq a :: tl -> "\tpushq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Popq a :: tl -> "\tpopq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Retq :: tl -> print_instrs tl
  | Xorq (a, b) :: tl -> "\txorq\t" ^ arg_to_x86 a ^ ",\t" ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Cmpq (a, b) :: tl -> "\tcmpq\t" ^ arg_to_x86 a ^ ",\t" ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Set (cmp, a) :: tl -> "\tset" ^ cmp_to_x86 cmp ^ "\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Movzbq (a, b) :: tl -> "\tmovzbq\t" ^ arg_to_x86 a ^ ",\t" ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Jmp a :: tl -> "\tjmp\t" ^ a ^ "\n" ^ (print_instrs tl)
  | JmpIf (cmp, a) :: tl -> "\tj" ^ cmp_to_x86 cmp ^ "\t" ^ a ^ "\n" ^ (print_instrs tl)
  | Label l :: tl -> l ^ ":\n" ^ (print_instrs tl)
  | _ -> invalid_instruction "invalid instruction"

let print_x86 program =
  match program with
  | AProgram (space, datatype, instrs) ->
    let beginning = "\t.globl " ^ os_label_prefix ^ "main\n" ^
                    os_label_prefix ^ "main:\n" ^
                    "\tpushq\t%rbp\n" ^
                    "\tmovq\t%rsp, %rbp\n" ^
                    (add_callee_save_registers callee_save_registers "pushq") ^
                    "\tsubq\t$" ^ (string_of_int (space + callee_save_stack_size)) ^ ", %rsp\n\n" in
    let middle = print_instrs instrs in
    let ending = "\n\tmovq\t%rax, %rdi\n" ^
                 "\tcallq\t" ^ os_label_prefix ^ "print_int\n" ^
                 "\taddq\t$" ^ (string_of_int (space + callee_save_stack_size)) ^ ",\t%rsp\n" ^
                 "\tmovq\t$0,\t%rax\n" ^
                 (add_callee_save_registers (List.rev callee_save_registers) "popq") ^
                 "\tpopq\t%rbp\n" ^
                 "\tretq\n" in
    (beginning ^ middle ^ ending)



let run_lex program =
    let stream = get_stream program `String in
    scan_all_tokens stream []


let run_parse program =
    let tokens = run_lex program in
    parse tokens


let run_uniquify program =
    let ast = run_parse program in
    uniquify ast


let run_typecheck program =
    let uniq = run_uniquify program in
    typecheck uniq


let run_flatten program =
    let typed = run_typecheck program in
    flatten typed


let run_select_instrs program =
    let flat = run_flatten program in
    select_instructions flat


let run_uncover_live program =
    let instr = run_select_instrs program in
    uncover_live instr


let run_build_inter program =
    let instr = run_uncover_live program in
    build_interference instr


let run_allocate_registers program =
    let instr = run_build_inter program in
    allocate_registers instr


let run_lower_conditionals program =
    let instr = run_allocate_registers program in
    lower_conditionals instr


let run_assign_homes program =
    let instr = run_lower_conditionals program in
    assign_homes instr


let run_patch_instructions program =
    let instr = run_assign_homes program in
    patch_instructions instr


let run_print_x86 program =
    let instr = run_patch_instructions program in
    print_x86 instr


