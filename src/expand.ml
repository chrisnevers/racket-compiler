open RProgram
open List

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
  | RBegin es -> expand_exp (RLet ("_", hd es, begin_to_let (tl es)))
  | RWhen (cnd, es) -> RIf (expand_exp_type cnd, expand_exp_type (make_tnone (RBegin es)), make_tnone RVoid)
  | RUnless (cnd, es) -> expand_exp (RWhen (make_tnone (RNot (cnd)), es))
  (* Expand inner expressions *)
  | RPrint e -> RPrint (expand_exp_type e)
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
  | _ -> exp

and expand_exp_type exp :rexp_type =
  match exp with
  | TypeIs (dt, e) -> TypeIs (dt, expand_exp e)

let expand program =
  match program with
  | RProgram (dt, defs, e) ->
    RProgram (dt, defs, expand_exp_type e)
