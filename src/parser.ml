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
