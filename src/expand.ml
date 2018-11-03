open RProgram
open List

let rec array_to_vector_type dt =
  match dt with
  | TypeArray a -> TypeVector [TypeInt; TypeArray (array_to_vector_type a)]
  | _ -> dt

let rec begin_to_let es =
  match es with
  | h :: [] -> h
  | h :: tl -> make_tnone (RLet ("_", h, begin_to_let tl))
  | [] -> make_tnone RVoid

and expand_exp exp :rexp =
  match exp with
  (* Expand sugars *)
  | RAnd (l, r) -> RIf (expand_exp_type l, expand_exp_type r, make_tbool (RBool false))
  | ROr (l, r) -> RIf (expand_exp_type l, make_tbool (RBool true), expand_exp_type r)
  | RWhen (cnd, es) -> RIf (expand_exp_type cnd, expand_exp_type (make_tnone (RBegin es)), make_tnone RVoid)
  | RUnless (cnd, es) -> expand_exp (RWhen (make_tnone (RNot (cnd)), es))
  (* Expand inner expressions *)
  | RPrint e -> RPrint (expand_exp_type e)
  | RArray es ->
    let len = make_tint (RInt (length es)) in
    let arr = make_tnone (RArray (map expand_exp_type es)) in
    RVector [len; arr]
  | RArraySet (a, i, e) -> RArraySet (expand_exp_type a, expand_exp_type i, expand_exp_type e)
  | RArrayRef (a, i) -> RArrayRef (expand_exp_type a, expand_exp_type i)
  | RVector es -> RVector (List.map expand_exp_type es)
  | RVectorRef (e, i) -> RVectorRef (expand_exp_type e, i)
  | RVectorSet (v, i, e) -> RVectorSet (expand_exp_type v, i, expand_exp_type e)
  | RVectorLength v -> RVectorLength (expand_exp_type v)
  | RNot e -> RNot (expand_exp_type e)
  | RIf (c, t, e) -> RIf (expand_exp_type c, expand_exp_type t, expand_exp_type e)
  | RCmp (o, l, r) -> RCmp (o, expand_exp_type l, expand_exp_type r)
  | RUnOp (o, e) -> RUnOp (o, expand_exp_type e)
  | RBinOp (o, l, r) -> RBinOp (o, expand_exp_type l, expand_exp_type r)
  | RLet (v, i, b) -> RLet (v, expand_exp_type i, expand_exp_type b)
  | RWhile (c, e) -> RWhile (expand_exp_type c, expand_exp_type e)
  | RApply (e, args) -> RApply (expand_exp_type e, List.map (fun a -> expand_exp_type a) args)
  | RLambda (args, ret, e) -> RLambda (args, ret, expand_exp_type e)
  | _ -> exp

and expand_exp_type exp :rexp_type =
  match exp with
  | TypeIs (dt, RBegin es) -> expand_exp_type (begin_to_let es)
  | TypeIs (dt, e) -> TypeIs (dt, expand_exp e)

let rec expand_defs defs =
  match defs with
  | RDefine (id, args, ret_type, body) :: t ->
    let new_args = map (fun (id, dt) -> (id, array_to_vector_type dt)) args in
    let new_ret = array_to_vector_type ret_type in
    let new_def = RDefine (id, new_args, new_ret, expand_exp_type body) in
    new_def :: expand_defs t
  | [] -> []

let expand program =
  match program with
  | RProgram (dt, defs, e) ->
    RProgram (dt, expand_defs defs, expand_exp_type e)
