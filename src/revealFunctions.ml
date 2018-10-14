open RProgram
open List

let rec reveal_typed_exp exp =
  match exp with
  | TypeIs (Some (TypeFunction (args, ret)), RVar label) ->
    TypeIs (Some (TypeFunction (args, ret)), RFunctionRef label)
  | TypeIs (dt, e) -> TypeIs (dt, reveal_exp e)

and reveal_exp exp =
  match exp with
  | RVector es -> RVector (map (fun e -> reveal_typed_exp e) es)
  | RVectorRef (e, i) -> RVectorRef (reveal_typed_exp e, i)
  | RVectorSet (v, i, e) -> RVectorSet (reveal_typed_exp v, i, reveal_typed_exp e)
  | RVectorLength e -> RVectorLength (reveal_typed_exp e)
  | RAnd (l, r) -> RAnd (reveal_typed_exp l, reveal_typed_exp r)
  | ROr (l, r) -> ROr (reveal_typed_exp l, reveal_typed_exp r)
  | RNot e -> RNot (reveal_typed_exp e)
  | RIf (c, t, e) -> RIf (reveal_typed_exp c, reveal_typed_exp t, reveal_typed_exp e)
  | RCmp (o, l, r) -> RCmp (o, reveal_typed_exp l, reveal_typed_exp r)
  | RUnOp (o, e) -> RUnOp (o, reveal_typed_exp e)
  | RBinOp (o, l, r) -> RBinOp (o, reveal_typed_exp l, reveal_typed_exp r)
  | RLet (v, i, b) -> RLet (v, reveal_typed_exp i, reveal_typed_exp b)
  | RBegin es -> RBegin (map (fun e -> reveal_typed_exp e) es)
  | RWhen (c, es) -> RWhen (c, map (fun e -> reveal_typed_exp e) es)
  | RUnless (c, es) -> RUnless (c, map (fun e -> reveal_typed_exp e) es)
  | RPrint e -> RPrint (reveal_typed_exp e)
  | RWhile (c, e) -> RWhile (c, reveal_typed_exp e)
  | RApply (id, es) -> RApply (id, map (fun e -> reveal_typed_exp e) es)
  | RVar _ -> exp
  | RInt _ -> exp
  | RBool _ -> exp
  | RVoid -> exp
  | RFunctionRef _ -> exp
  | RCollect _ -> exp
  | RAllocate _ -> exp
  | RGlobalValue _ -> exp
  | RRead -> exp

let rec reveal_defs defs =
  match defs with
  | RDefine (id, args, ret, body) :: t ->
    RDefine (id, args, ret, reveal_typed_exp body) :: reveal_defs t
  | [] -> []

let reveal_functions program =
  match program with
  | RProgram (dt, defs, exp) ->
    RProgram (dt, reveal_defs defs, reveal_typed_exp exp)
