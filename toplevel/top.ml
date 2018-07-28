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
  | TEOF -> "EOF"

let print_tokens tokens =
  List.iter (fun t -> print_string ((string_of_token t) ^ " ")) tokens;
  print_string "\n";
(* rProgram *)

type datatype =
  | TypeInt
  | TypeBool
  | TypeUnit

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

type rprogram =
  | RProgram of datatype * rexp

let string_of_datatype dt : string =
  match dt with
  | TypeInt -> "int"
  | TypeBool -> "bool"
  | TypeUnit -> "()"

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
  match a with
  | CInt i -> "Int " ^ (string_of_int i)
  | CVar v -> "Var " ^ v
  | CBool b -> "Bool " ^ (string_of_bool b)

let string_of_cexp e : string =
  match e with
  | CArg a -> "Arg " ^ (string_of_carg a)
  | CRead -> "Read"
  | CUnOp (o, a) -> "UnOp " ^ o ^ " " ^ (string_of_carg a)
  | CBinOp (o, l, r) -> "BinOp " ^ o ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  | CNot a -> "Not " ^ (string_of_carg a)
  | CCmp (cmp, l, r) -> "Cmp " ^ (string_of_ccmp cmp) ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)

let rec string_of_cstmts stmts : string =
  List.fold_left (fun acc s -> acc ^ string_of_cstmt s ^ " ") "" stmts

and string_of_cstmt a : string =
  "(" ^ (fun e ->
  match a with
  | CAssign (v, e) -> "Assign " ^ v ^ " " ^ (string_of_cexp e)
  | CReturn a -> "Return " ^ (string_of_carg a)
  | CIf (cnd, thn, els) -> "If " ^ (string_of_cexp cnd) ^ " " ^ (string_of_cstmts thn) ^ " " ^ (string_of_cstmts els)
  ) a
  ^ ")"

let string_of_string_list l : string =
  List.fold_left (fun acc s -> acc ^ s ^ " ") "" l

let print_cprogram program =
  match program with
  | CProgram (vars, dt, stmts) ->
    print_endline (
      "Program : " ^ (string_of_datatype dt) ^ 
      "\n\tVars: [" ^ (string_of_string_list vars) ^ "]" ^
      "\n\t[" ^ (string_of_cstmts stmts) ^ "]"
      )

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
    let stream = Stream.of_channel channel in
    let _ = close_in channel in stream
  | `String -> Stream.of_string src

(* Skips white space *)
let rec next_char stream : char = 
  let nc = Stream.next stream in
  match nc with
  | ' ' | '\t' | '\n' -> next_char stream
  | c -> c

let peek_char stream : char option = Stream.peek stream

let is_valid_id c : bool =
  is_alpha c || is_digit c || c = '_' || c = '?'

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
    | _         -> TVar acc

let get_cmp_op c : token =
  match c with
  | 't' -> TBool true
  | 'f' -> TBool false
  | _ -> lexer_error ("scan_token: Expected #t or #f but received #" ^ (Char.escaped c))

let scan_token stream : token =
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

let rec parse_exp tokens : rexp =
  let token = get_token tokens in
  match token with
  | TLParen ->
    let exp = parse_exp tokens in
    expect_token tokens TRParen; exp
  | TInt i -> RInt i
  | TVar v -> RVar v
  | TBool b -> RBool b
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

let parse_program tokens : rprogram =
  expect_token tokens TLParen;
  expect_token tokens TProgram;
  let exp = parse_exp tokens in
  expect_token tokens TRParen;
  expect_token tokens TEOF;
  RProgram (TypeUnit, exp)

let parse tokens =
  let token_list = ref tokens in
  parse_program token_list

(* uniquify *)


exception UniquifyError of string

let uniquify_error s = raise (UniquifyError s)

let get_var_name v table : string =
  try
    let count = Hashtbl.find table v in
    v ^ (string_of_int count)
  with Not_found -> uniquify_error ("get_var_name: Variable " ^ v ^ " is undefined")

let uniquify_name v table : string =
  try
    let count = (Hashtbl.find table v) + 1 in
    let _ = Hashtbl.replace table v count in v ^ (string_of_int count)
  with Not_found -> 
    let _ = Hashtbl.add table v 1 in v ^ "1"

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
  with Not_found -> typecheck_error ""

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
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same value"
  | RCmp (o, l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    (match o with
    | ">" | ">=" | "<" | "<=" -> 
      if ltype != TypeInt || rtype != TypeInt then TypeBool
      else typecheck_error ("typecheck_exp: " ^ o ^ "operates on integers")
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


let flatten_exp e : carg * cstmt list * string list =
  (CInt 0, [CReturn (CInt 0)], [])

let flatten program : cprogram =
  match program with
  | RProgram (dt, e) ->
    let (arg, stmts, vars) = flatten_exp e in
    CProgram (vars, dt, stmts)
