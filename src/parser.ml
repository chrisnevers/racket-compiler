open RProgram
open Token
open Helper
open List

exception ParserError of string
let parser_error s = raise (ParserError s)

let next_token tokens = hd !tokens

let rec peek_at tokens i : token =
  match i with
  | 1 -> hd !tokens
  | n when n > 1 -> peek_at (ref (tl !tokens)) (n - 1)
  | _ -> parser_error "peek_at takes a positive integer"

let get_token tokens =
  let token = next_token tokens in
  tokens := tl !tokens;
  token

let expect_token tokens expected =
  let actual = get_token tokens in
  if actual = expected then ()
  else parser_error ("Expected " ^ string_of_token expected ^ ", but received " ^ string_of_token actual)

let parse_id tokens =
  let token = get_token tokens in
  match token with
  | TVar id -> id
  | _ -> parser_error ("Expected variable, but received " ^ string_of_token token)

let parse_int tokens =
  let token = get_token tokens in
  match token with
  | TInt i -> i
  | _ -> parser_error ("Expected integer, but received " ^ string_of_token token)

let token_to_datatype token =
  match token with
  | TTypeInt -> TypeInt
  | TTypeBool -> TypeBool
  | TTypeVoid -> TypeVoid
  | _ -> parser_error "expected int, bool, or void"

let rec parse_types tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> []
  | _ ->
    let ty = parse_type tokens in
    ty :: parse_types tokens

and parse_type tokens =
  let token = get_token tokens in
  match token with
  | TTypeInt -> TypeInt
  | TTypeBool -> TypeBool
  | TTypeVoid -> TypeVoid
  | TLParen ->
    let ty = parse_inner_type tokens in
    expect_token tokens TRParen;
    ty
  | _ -> parser_error ("expected type but received " ^ string_of_token token)

and parse_inner_type tokens =
  let token = get_token tokens in
  match token with
  | TTypeVector ->
    let types = parse_types tokens in
    TypeVector types
  | TArrow ->
    let ret = parse_type tokens in
    TypeFunction ([], ret)
  | TTypeInt | TTypeBool | TTypeVoid ->
    let types = token_to_datatype token :: parse_function_types tokens in
    let ret = last types in
    let args = rm_last types in
    TypeFunction (args, ret)
  | _ -> parser_error "expected type"

and parse_function_types tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> []
  | TArrow ->
    expect_token tokens TArrow;
    let ty = parse_type tokens in
    ty :: parse_function_types tokens
  | _ -> parser_error "expected -> or )"

let parse_arg tokens =
  expect_token tokens TLBracket;
  let id = parse_id tokens in
  expect_token tokens TColon;
  let ty = parse_type tokens in
  expect_token tokens TRBracket;
  (id, ty)

let rec parse_args tokens =
  let next = next_token tokens in
  match next with
  | TLBracket ->
    let arg = parse_arg tokens in
    arg :: parse_args tokens
  | _ -> []

let rec parse_typed_exps tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> []
  | _ ->
    let exp = parse_typed_exp tokens in
    exp :: parse_typed_exps tokens

and parse_typed_exp tokens = TypeIs (None, parse_exp tokens)

and parse_exp tokens =
  let token = get_token tokens in
  match token with
  | TVar v -> RVar v
  | TInt i -> RInt i
  | TBool b -> RBool b
  | TLParen ->
    let ie = parse_inner_exp tokens in
    expect_token tokens TRParen;
    ie
  | _ -> parser_error "expected variable, int, bool, or ("

and parse_inner_exp tokens =
  let token = get_token tokens in
  match token with
  | TRead -> RRead
  | TVoid -> RVoid
  | TArithOp o ->
    let exp = parse_typed_exp tokens in
    let next = next_token tokens in
    begin
    match next with
    | TRParen -> RUnOp (o, exp)
    | _ ->
      let rhs = parse_typed_exp tokens in
      RBinOp (o, exp, rhs)
    end
  | TBegin ->
    let exps = parse_typed_exps tokens in
    RBegin exps
  | TWhen ->
    let cnd = parse_typed_exp tokens in
    let exps = parse_typed_exps tokens in
    RWhen (cnd, exps)
  | TUnless ->
    let cnd = parse_typed_exp tokens in
    let exps = parse_typed_exps tokens in
    RUnless (cnd, exps)
  | TVector ->
    let exps = parse_typed_exps tokens in
    RVector exps
  | TVectorRef ->
    let vec = parse_typed_exp tokens in
    let index = parse_int tokens in
    RVectorRef (vec, index)
  | TVectorSet ->
    let vec = parse_typed_exp tokens in
    let index = parse_int tokens in
    let exp = parse_typed_exp tokens in
    RVectorSet (vec, index, exp)
  | TVectorLength ->
    let vec = parse_typed_exp tokens in
    RVectorLength vec
  | TPrint ->
    let exp = parse_typed_exp tokens in
    RPrint exp
  | TWhile ->
    let cnd = parse_typed_exp tokens in
    let exp = parse_typed_exp tokens in
    RWhile (cnd, exp)
  | TLogOp "and" ->
    let lhs = parse_typed_exp tokens in
    let rhs = parse_typed_exp tokens in
    RAnd (lhs, rhs)
  | TLogOp "or" ->
    let lhs = parse_typed_exp tokens in
    let rhs = parse_typed_exp tokens in
    ROr (lhs, rhs)
  | TLogOp "not" ->
    let exp = parse_typed_exp tokens in
    RNot exp
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
    let lhs = parse_typed_exp tokens in
    let rhs = parse_typed_exp tokens in
    RCmp (o, lhs, rhs)
  | TLet ->
    expect_token tokens TLParen;
    expect_token tokens TLBracket;
    let id = parse_id tokens in
    let exp = parse_typed_exp tokens in
    expect_token tokens TRBracket;
    expect_token tokens TRParen;
    let body = parse_typed_exp tokens in
    RLet (id, exp, body)
  | TIf ->
    let cnd = parse_typed_exp tokens in
    let thn = parse_typed_exp tokens in
    let els = parse_typed_exp tokens in
    RIf (cnd, thn, els)
  | TLambda ->
    expect_token tokens TLParen;
    let args = parse_args tokens in
    expect_token tokens TRParen;
    expect_token tokens TColon;
    let ret = parse_type tokens in
    let exp = parse_typed_exp tokens in
    RLambda (args, ret, exp)
  | TVar id ->
    let exps = parse_typed_exps tokens in
    RApply (TypeIs (None, RVar id), exps)
  | _ -> parser_error ("Error parsing exp. Did not expect " ^ string_of_token token)

let parse_def tokens =
  expect_token tokens TLParen;
  expect_token tokens TDefine;
  expect_token tokens TLParen;
  let id = parse_id tokens in
  let args = parse_args tokens in
  expect_token tokens TRParen;
  expect_token tokens TColon;
  let ret = parse_type tokens in
  let exp = parse_typed_exp tokens in
  expect_token tokens TRParen;
  RDefine (id, args, ret, exp)

let rec parse_defs tokens =
  let token = peek_at tokens 2 in
  match token with
  | TDefine ->
    let def = parse_def tokens in
    def :: parse_defs tokens
  | _ -> []

let parse_program tokens =
  let defs = parse_defs tokens in
  let exp = parse_typed_exp tokens in
  expect_token tokens TEOF;
  RProgram (None, defs, exp)

let parse program =
  let tokens = ref program in
  parse_program tokens
