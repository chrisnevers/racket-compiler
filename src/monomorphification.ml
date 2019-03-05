open RProgram
open Gensym
open List

exception MonomorphicationError of string
let mono_error msg = raise (MonomorphicationError msg)

(*
  Changes all occurences of type parameter to the given argument
  (lambda ([x : A]): A x) => (lambda ([x : Bool]): Bool x)
 *)
let rec replace_dt actual variable inst =
  let _rec t = replace_dt t variable inst in
  match actual with
  | TypeVar id when id = variable -> inst
  | TypeUser id when id = variable -> inst
  | TypeForAll (id, t) when id = variable -> _rec t
  | TypeForAll (id, t) -> TypeForAll (id, _rec t)
  | TypeArray t -> TypeArray (_rec t)
  | TypeVector ts -> TypeVector (map _rec ts)
  | TypeFunction (ts, t) -> TypeFunction (map _rec ts, _rec t)
  | TypePlus (l, r) -> TypePlus (_rec l, _rec r)
  | TypeFix t -> TypeFix (_rec t)
  | _ -> actual

let rec instantiate_type exp var_ty inst_ty =
  (* recursive helpers *)
  let _rec te = match te with
    | TypeIs (Some dt, e) ->
      TypeIs (Some (replace_dt dt var_ty inst_ty), instantiate_type e var_ty inst_ty)
    | TypeIs (dt, e) -> TypeIs (dt, instantiate_type e var_ty inst_ty)
  in
  let _recs es = map _rec es in
  (* Replaces args *)
  let instantiate_types (arg, dt) = (arg, replace_dt dt var_ty inst_ty) in
  match exp with
  | RLambda (args, ret, e) ->
    (* mono_some_type ret (var_ty, inst_ty); *)
    let new_args = map instantiate_types args in
    let new_ret = replace_dt ret var_ty inst_ty in
    RLambda (new_args, new_ret, _rec e)
  (* Process inner expressions of everything else *)
  | RTyLambda (ty, e) -> RTyLambda (ty, _rec e)
  | RVar _ | RInt _ | RChar _ | RBool _
  | RCollect _ | RAllocate _ | RGlobalValue _ | RRead
  | RVoid | RFunctionRef _ | RDatatype _ -> exp
  | RArray (i, es) -> RArray (i, map _rec es)
  | RArraySet (a, i, e) -> RArraySet (_rec a, _rec i, _rec e)
  | RArrayRef (a, i) -> RArrayRef (_rec a, _rec i)
  | RApply (e, args) -> RApply (_rec e, map _rec args)
  | RVector es -> RVector (map _rec es)
  | RVectorRef (v, i) -> RVectorRef (_rec v, i)
  | RVectorSet (v, i, e) -> RVectorSet (_rec v, i, _rec e)
  | RVectorLength v -> RVectorLength (_rec v)
  | RAnd (l, r) -> RAnd (_rec l, _rec r)
  | ROr (l, r) -> ROr (_rec l, _rec r)
  | RNot e -> RNot (_rec e)
  | RIf (c, t, e) -> RIf (_rec c, _rec t, _rec e)
  | RCmp (o, l, r) -> RCmp (o, _rec l, _rec r)
  | RUnOp (o, e) -> RUnOp (o, _rec e)
  | RBinOp (o, l, r) -> RBinOp (o, _rec l, _rec r)
  | RLet (v, i, b) -> RLet (v, _rec i, _rec b)
  | RBegin es -> RBegin (map _rec es)
  | RWhen (c, es) -> RWhen (_rec c, map _rec es)
  | RUnless (c, es) -> RUnless (_rec c, map _rec es)
  | RPrint e -> RPrint (_rec e)
  | RWhile (c, e) -> RWhile (_rec c, _rec e)
  | RLet (v, i, b) -> RLet (v, _rec i, _rec b)
  | RCase (m, cases) -> RCase (_rec m, map (fun (a, b) -> (_rec a, _rec b)) cases)
  | RInl (e, dt) -> RInl (_rec e, replace_dt dt var_ty inst_ty)
  | RInr (dt, e) -> RInr (replace_dt dt var_ty inst_ty, _rec e)
  | RIsInl e -> RIsInl (_rec e)
  | RIsInr e -> RIsInr (_rec e)
  | RFold e -> RFold (_rec e)
  | RUnfold e -> RUnfold (_rec e)
  | _ -> mono_error ("instantiate_types: unexpected expression - " ^ string_of_rexp exp)

let rec m_tyexp exp tbl =
  match exp with
  | TypeIs (dt, e) -> TypeIs (dt, m_exp e tbl)

and m_exp exp tbl =
  let _rec e = m_tyexp e tbl in
  match exp with
  | RLet (id, TypeIs (_, (RTyLambda (ty, te) as ie)), be) ->
    let _ = Hashtbl.add tbl id ie in
    let TypeIs (_, new_be) = _rec be in new_be
  | RInst (TypeIs (_, RVar id), dt) ->
    let RTyLambda (ty, te) = Hashtbl.find tbl id in
    let TypeIs (_, e) = te in
    instantiate_type e ty dt
  | RInst (TypeIs (_, RTyLambda (ty, te)), dt) ->
    let TypeIs (_, e) = te in
    instantiate_type e ty dt
  | RInst (ie, dt) -> m_exp (RInst (_rec ie, dt)) tbl
  (* Do nothing but process inner expressions *)
  | RTyLambda (ty, te) -> RTyLambda (ty, _rec te)
  | RVar _ | RInt _ | RChar _ | RBool _
  | RCollect _ | RAllocate _ | RGlobalValue _ | RRead
  | RVoid | RFunctionRef _ | RDatatype _ -> exp
  | RArray (i, es) -> RArray (i, map _rec es)
  | RArraySet (a, i, e) -> RArraySet (_rec a, _rec i, _rec e)
  | RArrayRef (a, i) -> RArrayRef (_rec a, _rec i)
  | RApply (e, args) -> RApply (_rec e, map _rec args)
  | RVector es -> RVector (map _rec es)
  | RVectorRef (v, i) -> RVectorRef (_rec v, i)
  | RVectorSet (v, i, e) -> RVectorSet (_rec v, i, _rec e)
  | RVectorLength v -> RVectorLength (_rec v)
  | RAnd (l, r) -> RAnd (_rec l, _rec r)
  | ROr (l, r) -> ROr (_rec l, _rec r)
  | RNot e -> RNot (_rec e)
  | RIf (c, t, e) -> RIf (_rec c, _rec t, _rec e)
  | RCmp (o, l, r) -> RCmp (o, _rec l, _rec r)
  | RUnOp (o, e) -> RUnOp (o, _rec e)
  | RBinOp (o, l, r) -> RBinOp (o, _rec l, _rec r)
  | RLet (v, i, b) -> RLet (v, _rec i, _rec b)
  | RBegin es -> RBegin (map _rec es)
  | RWhen (c, es) -> RWhen (_rec c, map _rec es)
  | RUnless (c, es) -> RUnless (_rec c, map _rec es)
  | RPrint e -> RPrint (_rec e)
  | RWhile (c, e) -> RWhile (_rec c, _rec e)
  | RLambda (args, ret, e) -> RLambda (args, ret, _rec e)
  | RCase (m, cases) -> RCase (_rec m, map (fun (a, b) -> (_rec a, _rec b)) cases)
  | RInl (e, dt) -> RInl (_rec e, dt)
  | RInr (dt, e) -> RInr (dt, _rec e)
  | RFold e -> RFold (_rec e)
  | RUnfold e -> RUnfold (_rec e)
  | RIsInl e -> RIsInl (_rec e)
  | RIsInr e -> RIsInr (_rec e)
  | _ -> mono_error ("m_exp: Unexpected expression: " ^ string_of_rexp exp)

let rec m_def defs tbl =
  match defs with
  | [] -> []
  | RDefine (id, args, ret, TypeIs (edt, (RTyLambda (ty, te) as ie))) :: t ->
    let _ = Hashtbl.add tbl id ie in
    let new_body = m_tyexp (TypeIs (edt, ie)) tbl in
    RDefine (id, args, ret, new_body) ::
    m_def t tbl
  | RDefine (id, args, ret, body) :: t ->
    let new_body = m_tyexp body tbl in
    RDefine (id, args, ret, new_body) :: m_def t tbl
  | def :: t -> def :: m_def t tbl

let rec mono_types defs =
  match defs with
  | [] -> []
  | RDefType (id, l, r, vars, dt) :: t ->
    if vars = [] then
      RDefType (id, l, r, vars, dt) :: mono_types t
    else
      mono_types t
  | d :: t -> d :: mono_types t

let monomorphize program =
  match program with
  | RProgram (dt, defs, exp) ->
    let tbl = Hashtbl.create 10 in
    let new_defs = m_def defs tbl in
    let new_exp = m_tyexp exp tbl in
    (* let mono_defs = mono_types defs in *)
    RProgram (dt, new_defs, new_exp)
