open List
open RProgram
open Token
open Helper

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
    (match exp with
    | RVar v -> parse_func_call v tokens
    | _ -> expect_token tokens TRParen; exp)
  | TVoid -> RVoid
  | TInt i -> RInt i
  | TVar v -> RVar v
  | TBool b -> RBool b
  | TBegin ->
    let exps = parse_inner_exps tokens in
    RBegin exps
    | TWhen ->
    let cnd = parse_typed_exp tokens in
    let exps = parse_inner_exps tokens in
    RWhen (cnd, exps)
  | TUnless ->
    let cnd = parse_typed_exp tokens in
    let exps = parse_inner_exps tokens in
    RUnless (cnd, exps)
  | TVector ->
    let exps = parse_inner_exps tokens in
    RVector exps
  | TVectorRef ->
    let exp = parse_typed_exp tokens in
    let index = parse_int tokens in
    RVectorRef(exp, index)
  | TVectorSet ->
    let e1 = parse_typed_exp tokens in
    let index = parse_int tokens in
    let e2 = parse_typed_exp tokens in
    RVectorSet(e1, index, e2)
  | TVectorLength ->
    let e = parse_typed_exp tokens in
    RVectorLength e
  | TRead -> RRead
  | TPrint ->
    let exp = parse_typed_exp tokens in
    RPrint exp
  | TWhile ->
    let cnd = parse_typed_exp tokens in
    let exp = parse_typed_exp tokens in
    RWhile (cnd, exp)
  | TArithOp o ->
    let exp = parse_typed_exp tokens in
    (match next_token tokens with
    | TRParen -> RUnOp (o, exp)
    | _ ->
      let exp2 = parse_typed_exp tokens in
      RBinOp(o, exp, exp2))
  | TLogOp "and" ->
    let l = parse_typed_exp tokens in
    let r = parse_typed_exp tokens in
    RAnd (l, r)
  | TLogOp "or" ->
    let l = parse_typed_exp tokens in
    let r = parse_typed_exp tokens in
    ROr (l, r)
  | TLogOp "not" ->
    let exp = parse_typed_exp tokens in RNot exp
  | TPos ->
    let exp = parse_typed_exp tokens in
    RCmp (">", exp, TypeIs (None, RInt 0))
  | TNeg ->
    let exp = parse_typed_exp tokens in
    RCmp ("<", exp, TypeIs (None, RInt 0))
  | TZero ->
    let exp = parse_typed_exp tokens in
    RCmp ("eq?", exp, TypeIs (None, RInt 0))
  | TCmpOp o ->
    let l = parse_typed_exp tokens in
    let r = parse_typed_exp tokens in
    RCmp (o, l, r)
  | TLet ->
    expect_token tokens TLParen;
    expect_token tokens TLBracket;
    let v = parse_var tokens in
    let i = parse_typed_exp tokens in
    expect_token tokens TRBracket;
    expect_token tokens TRParen;
    let b = parse_typed_exp tokens in
    RLet (v, i, b)
  | TIf ->
    let cnd = parse_typed_exp tokens in
    let thn = parse_typed_exp tokens in
    let els = parse_typed_exp tokens in
    RIf (cnd, thn, els)
  | _ -> parser_error ("Did not expect token " ^ (string_of_token token))

and parse_typed_exp tokens =
  TypeIs (None, parse_exp tokens)

and parse_inner_exps tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> []
  | _ ->
    let exp = parse_typed_exp tokens in
    exp :: parse_inner_exps tokens

and parse_func_call v tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> expect_token tokens TRParen; RApply (TypeIs (None, RVar v), [])
  | _ ->
    let args = parse_inner_exps tokens in
    expect_token tokens TRParen;
    RApply (TypeIs (None, RVar v), args)

(* Vector and Func types are wrapped in parens *)
let is_type tokens =
  let next = next_token tokens in
  match next with
  | TTypeInt | TTypeBool | TTypeVoid | TTypeVector -> true
  | TLParen ->
    let nt = hd (tl !tokens) in
    (match nt with
    | TTypeVector | TTypeInt | TTypeBool | TTypeVoid -> true
    | _ -> false)
  | _ -> false

let token_to_type token =
  match token with
  | TTypeInt -> TypeInt
  | TTypeBool -> TypeBool
  | TTypeVoid -> TypeVoid
  | _ -> parser_error "expected int, bool, or void"

let rec parse_inner_type tokens =
  let token = get_token tokens in
  match token with
  | TTypeVector ->
    let types = parse_types tokens in
    TypeVector types
  | TTypeInt | TTypeBool | TTypeVoid ->
    let arg_types = parse_func_types tokens in
    TypeFunction (arg_types, token_to_type token)
  | TArrow ->
    let ret_type = parse_type tokens in
    TypeFunction ([], ret_type)
  | _ -> parser_error "expected type or arrow"

and parse_func_types tokens =
  let next = next_token tokens in
  match next with
  | TArrow ->
    expect_token tokens TArrow;
    let arg_type = parse_type tokens in
    arg_type :: parse_func_types tokens
  | TRParen -> []
  | _ -> parser_error "expected -> or )"

and parse_type tokens =
  let token = get_token tokens in
  match token with
  | TTypeInt  -> TypeInt
  | TTypeBool -> TypeBool
  | TTypeVoid -> TypeVoid
  | TLParen   ->
    let itype = parse_inner_type tokens in
    expect_token tokens TRParen;
    itype
  | _ -> parser_error ("expected a type but received: " ^ (string_of_token token))

and parse_types tokens =
  match is_type tokens with
  | true ->
    let arg_type = parse_type tokens in
    arg_type :: parse_types tokens
  | false -> []

let rec parse_arg tokens =
  expect_token tokens TLBracket;
  let id = parse_var tokens in
  expect_token tokens TColon;
  let id_type = parse_type tokens in
  expect_token tokens TRBracket;
  (id, id_type)

and parse_args tokens =
  let next = next_token tokens in
  match next with
  | TLBracket ->
    let arg = parse_arg tokens in
    arg :: parse_args tokens
  | _ -> []

let parse_definition tokens =
  expect_token tokens TLParen;
  expect_token tokens TDefine;
  expect_token tokens TLParen;
  let id = parse_var tokens in
  let args = parse_args tokens in
  expect_token tokens TRParen;
  expect_token tokens TColon;
  let ret_type = parse_type tokens in
  let body = parse_typed_exp tokens in
  expect_token tokens TRParen;
  RDefine (id, args, ret_type, body)

let rec parse_definitions tokens =
  let token = hd (tl !tokens) in
  match token with
  | TDefine ->
    let def = parse_definition tokens in
    def :: parse_definitions tokens
  | _ -> []

let parse_program tokens : rprogram =
  expect_token tokens TLParen;
  expect_token tokens TProgram;
  let defs = parse_definitions tokens in
  let exp = parse_typed_exp tokens in
  expect_token tokens TRParen;
  expect_token tokens TEOF;
  RProgram (None, defs, exp)

let parse tokens =
  let token_list = ref tokens in
  parse_program token_list
