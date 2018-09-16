open List
open RProgram
open Token

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
  | TLogOp "or" ->
    let l = parse_exp tokens in
    let r = parse_exp tokens in
    ROr (l, r)
  | TLogOp "not" ->
    let exp = parse_exp tokens in RNot exp
  | TPos ->
    let exp = parse_exp tokens in
    RCmp (">", exp, RInt 0)
  | TNeg ->
    let exp = parse_exp tokens in
    RCmp ("<", exp, RInt 0)
  | TZero ->
    let exp = parse_exp tokens in
    RCmp ("eq?", exp, RInt 0)
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
