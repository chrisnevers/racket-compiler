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
  | TTypeChar -> TypeChar
  | TTypeBool -> TypeBool
  | TTypeVoid -> TypeVoid
  | TVar id -> TypeVar id
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
  | TTypeChar -> TypeChar
  | TTypeBool -> TypeBool
  | TTypeVoid -> TypeVoid
  | TVar id -> TypeVar id
  | TLParen ->
    let ty = parse_inner_type tokens in
    expect_token tokens TRParen;
    ty
  | _ -> parser_error ("expected type but received " ^ string_of_token token)

and parse_inner_type tokens =
  let token = get_token tokens in
  match token with
  | TTypeArray ->
    let atype = parse_type tokens in
    TypeArray atype
  | TTypeVector ->
    let types = parse_types tokens in
    TypeVector types
  | TArrow ->
    let ret = parse_types tokens in
    TypeFunction (rm_last ret, last ret)
  | TTypeForAll ->
    let id = parse_id tokens in
    let ty = parse_type tokens in
    TypeForAll (id, ty)
  | TTypeFix ->
    let ty = parse_type tokens in
    TypeFix ty
  | _ -> parser_error "expected type"

let parse_arg tokens =
  expect_token tokens TLParen;
  let id = parse_id tokens in
  expect_token tokens TColon;
  let ty = parse_type tokens in
  expect_token tokens TRParen;
  (id, ty)

let rec parse_args tokens =
  let next = next_token tokens in
  match next with
  | TLParen ->
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
  | TChar c -> RChar c
  | TBool b -> RBool b
  | TLParen ->
    let exp = parse_inner_exp tokens in
    expect_token tokens TRParen;
    exp
  | _ -> parser_error "expected variable, int, bool, or ("

and parse_exp_tail tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> expect_token tokens TRParen; []
  | _ ->
    let exp = parse_typed_exp tokens in
    exp :: parse_exp_tail tokens

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
  | TArray ->
    let exps = parse_typed_exps tokens in
    RArray (length exps, exps)
  | TArraySet ->
    let arr = parse_typed_exp tokens in
    let index = parse_typed_exp tokens in
    let exp = parse_typed_exp tokens in
    RArraySet (arr, index, exp)
  | TArrayRef ->
    let arr = parse_typed_exp tokens in
    let index = parse_typed_exp tokens in
    RArrayRef (arr, index)
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
  | TTyLambda ->
    let id = parse_id tokens in
    let exp = parse_typed_exp tokens in
    RTyLambda (id, exp)
  | TInst ->
    let exp = parse_typed_exp tokens in
    let ty = parse_type tokens in
    RInst (exp, ty)
  | TVar id ->
    let exps = parse_typed_exps tokens in
    RApply (TypeIs (None, RVar id), exps)
  | TLParen ->
    let next = next_token tokens in
    begin
    match next with
    | TLambda ->
      let lambda = parse_inner_exp tokens in
      expect_token tokens TRParen;
      let exps = parse_typed_exps tokens in
      RApply (TypeIs (None, lambda), exps)
    | TInst ->
      expect_token tokens TInst;
      let exp = parse_typed_exp tokens in
      let ty = parse_type tokens in
      expect_token tokens TRParen;
      let exps = parse_typed_exps tokens in
      RApply (TypeIs (None, RInst (exp, ty)), exps)
    | _ -> parser_error "Expected (exp exp*): First argument must be lambda expression or variable in apply"
    end
  | TCase ->
    let expr = parse_typed_exp tokens in
    let cases = parse_cases tokens in
    RCase (expr, cases)
  | TIsInl ->
    let expr = parse_typed_exp tokens in
    RIsInl expr
  | TIsInr ->
    let expr = parse_typed_exp tokens in
    RIsInr expr
  | _ -> parser_error ("Error parsing exp. Did not expect " ^ string_of_token token)

and parse_cases tokens =
  let next = next_token tokens in
  match next with
  | TLParen ->
    expect_token tokens TLParen;
    let case = parse_typed_exp tokens in
    let do_this = parse_typed_exp tokens in
    expect_token tokens TRParen;
    (case, do_this) :: parse_cases tokens
  | _ -> []

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

let rec parse_sub_type tokens =
  let next = next_token tokens in
  match next with
  | TLParen ->
    expect_token tokens TLParen;
    let id = parse_id tokens in
    let next = next_token tokens in
    (* If no type specified, its type unit *)
    let ty = if next != TRParen then parse_type tokens else TypeVector [] in
    expect_token tokens TRParen;
    (id, ty)
  | _ -> parser_error "Expected [ while parsing variant"

let rec parse_forall tokens =
  let next = next_token tokens in
  match next with
  | TVar id ->
    expect_token tokens (TVar id);
    id :: parse_forall tokens
  | _ -> []

let rec get_type_var_dt vars dt =
  match vars with
  | id :: [] -> TypeForAll (id, dt)
  | id :: t -> TypeForAll (id, get_type_var_dt t dt)
  | [] -> dt

let rec get_wrapped_ty_con vars dt exp =
  match vars with
  | [] -> exp
  | id :: t -> TypeIs (Some (get_type_var_dt vars dt), RTyLambda (id, get_wrapped_ty_con t dt exp))

let is_unit ty = ty = TypeVector []

let parse_def_type tokens =
  (* Parse type defs *)
  expect_token tokens TLParen;
  expect_token tokens TDefineType;
  let type_id = parse_id tokens in
  let forall = parse_forall tokens in
  let (l_id, l_ty) = parse_sub_type tokens in
  let (r_id, r_ty) = parse_sub_type tokens in
  expect_token tokens TRParen;
  (* Create Types *)
  let l_unit = is_unit l_ty in
  let r_unit = is_unit r_ty in
  let plus_ty = TypePlus (l_ty, r_ty) in
  let fix_ty = TypeFix (TypeForAll (type_id, plus_ty)) in
  (* Polymorphic constructors wrapped in lambda - unit types have no args *)
  let l_lam_ty = TypeFunction ((if l_unit then [] else [l_ty]), fix_ty) in
  let r_lam_ty = TypeFunction ((if r_unit then [] else [r_ty]), fix_ty) in
  (* If unit, we don't have a parameter *)
  let inl_ie = if l_unit then RVector [] else RVar "x" in
  let inr_ie = if r_unit then RVector [] else RVar "x" in
  (* Construct the (Fold (Inl ...)) or (Fold (Inr ...)) *)
  let inl_e = RInl (TypeIs (Some l_ty, inl_ie), r_ty) in
  let inr_e = RInr (l_ty, TypeIs (Some r_ty, inr_ie)) in
  let l_fold = TypeIs (Some fix_ty, RFold (TypeIs (Some plus_ty, inl_e))) in
  let r_fold = TypeIs (Some fix_ty, RFold (TypeIs (Some plus_ty, inr_e))) in
  (* If polymorphic wrap (Fold ...) in a lambda *)
  let l_args = if l_unit then [] else [("x", l_ty)] in
  let l_e = if forall = [] then l_fold else
    TypeIs (Some l_lam_ty, RLambda (l_args, fix_ty, l_fold))
  in
  let r_args = if r_unit then [] else [("x", r_ty)] in
  let r_e = if forall = [] then r_fold
    else TypeIs (Some r_lam_ty, RLambda (r_args, fix_ty, r_fold))
  in
  (* Wrap constructor in necessary (TyLambda ...)'s *)
  let l_exp = if forall = [] then l_e else get_wrapped_ty_con forall l_lam_ty l_e in
  let r_exp = if forall = [] then r_e else get_wrapped_ty_con forall r_lam_ty r_e in
  (* If type is not polymorphic, the return type is (TypeFix (TypeForAll (...))) *)
  let l_bdt = if forall = [] then fix_ty else l_lam_ty in
  let r_bdt = if forall = [] then fix_ty else r_lam_ty in
  (* If type is polymorphic, the return type is wrapped in (ForAll ({type_var}, ...)) *)
  let l_dt = if forall = [] then l_bdt else get_type_var_dt forall l_bdt in
  let r_dt = if forall = [] then r_bdt else get_type_var_dt forall r_bdt in
  (* Are any arguments needed for the type constructor functions *)
  let l_args = if l_unit then [] else [("x", l_ty)] in
  let r_ards = if r_unit then [] else [("x", r_ty)] in
  RDefType (type_id, l_id, r_id, forall, fix_ty) ::
  RDefine (l_id, l_args, l_dt, l_exp) ::
  RDefine (r_id, r_ards, r_dt, r_exp) ::
  []

let rec parse_defs tokens =
  let token = peek_at tokens 2 in
  match token with
  | TDefine ->
    let def = parse_def tokens in
    def :: parse_defs tokens
  | TDefineType ->
    let defs = parse_def_type tokens in
    defs @ parse_defs tokens
  | _ -> []

let parse_program tokens =
  let defs = parse_defs tokens in
  let exp = parse_typed_exp tokens in
  expect_token tokens TEOF;
  RProgram (None, defs, exp)

let parse program =
  let tokens = ref program in
  parse_program tokens
