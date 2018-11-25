open RProgram
open List

exception ExpandError of string
let expand_error msg = raise (ExpandError msg)

let rec array_to_vector_type dt =
  match dt with
  | TypeArray a -> TypeVector [TypeInt; TypeArray (array_to_vector_type a)]
  | _ -> dt

let expand_user_type ty gamma =
  match ty with
  | (id, TypeUser s) -> begin try
      let (side, dt) = Hashtbl.find gamma s in
      (id, dt)
    with Not_found -> expand_error "Type has not been defined" end
  | _ -> ty

let rec begin_to_let es =
  match es with
  | h :: [] -> h
  | h :: tl -> make_tnone (RLet ("_", h, begin_to_let tl))
  | [] -> make_tnone RVoid

and expand_exp exp gamma : rexp =
  match exp with
  (* Expand sugars *)
  | RAnd (l, r) -> RIf (expand_exp_type l gamma, expand_exp_type r gamma, make_tbool (RBool false))
  | ROr (l, r) -> RIf (expand_exp_type l gamma, make_tbool (RBool true), expand_exp_type r gamma)
  | RWhen (cnd, es) -> RIf (expand_exp_type cnd gamma, expand_exp_type (make_tnone (RBegin es)) gamma, make_tnone RVoid)
  | RUnless (cnd, es) -> expand_exp (RWhen (make_tnone (RNot (cnd)), es)) gamma
  (* Expand inner expressions *)
  | RPrint e -> RPrint (expand_exp_type e gamma)
  | RArray (len, es) -> RArray (len, map (fun e -> expand_exp_type e gamma) es)
  | RArraySet (a, i, e) -> RArraySet (expand_exp_type a gamma, expand_exp_type i gamma, expand_exp_type e gamma)
  | RArrayRef (a, i) -> RArrayRef (expand_exp_type a gamma, expand_exp_type i gamma)
  | RVector es -> RVector (map (fun e -> expand_exp_type e gamma) es)
  | RVectorRef (e, i) -> RVectorRef (expand_exp_type e gamma, i)
  | RVectorSet (v, i, e) -> RVectorSet (expand_exp_type v gamma, i, expand_exp_type e gamma)
  | RVectorLength v -> RVectorLength (expand_exp_type v gamma)
  | RNot e -> RNot (expand_exp_type e gamma)
  | RIf (c, t, e) -> RIf (expand_exp_type c gamma, expand_exp_type t gamma, expand_exp_type e gamma)
  | RCmp (o, l, r) -> RCmp (o, expand_exp_type l gamma, expand_exp_type r gamma)
  | RUnOp (o, e) -> RUnOp (o, expand_exp_type e gamma)
  | RBinOp (o, l, r) -> RBinOp (o, expand_exp_type l gamma, expand_exp_type r gamma)
  | RLet (v, i, b) -> RLet (v, expand_exp_type i gamma, expand_exp_type b gamma)
  | RWhile (c, e) -> RWhile (expand_exp_type c gamma, expand_exp_type e gamma)
  | RApply (e, args) -> RApply (expand_exp_type e gamma, map (fun a -> expand_exp_type a gamma) args)
  | RLambda (args, ret, e) -> RLambda (map (fun a -> expand_user_type a gamma) args, ret, expand_exp_type e gamma)
  | RCase (e, cases) -> RCase (expand_exp_type e gamma, expand_user_cases cases gamma)
  | RInl (e, dt)  -> RInl (expand_exp_type e gamma, dt)
  | RInr (dt, e) -> RInr (dt, expand_exp_type e gamma)
  | _ -> exp

and expand_exp_type exp gamma : rexp_type =
  match exp with
  | TypeIs (dt, RBegin es) -> expand_exp_type (begin_to_let es) gamma
  | TypeIs (dt, e) -> TypeIs (dt, expand_exp e gamma)

and expand_user_cases cases gamma =
  match cases with
  | (TypeIs (_, RApply (id, [TypeIs (_, arg)])), exp) :: t ->
    let TypeIs (_, RVar name) = id in
    let (Some side, TypePlus (l_ty, r_ty)) = Hashtbl.find gamma name in
    let dt = Some (TypePlus (l_ty, r_ty)) in
    let ncnd = begin match side with
    | Left  -> TypeIs (dt, RInl (TypeIs (Some l_ty, arg), r_ty))
    | Right -> TypeIs (dt, RInr (l_ty, TypeIs (Some r_ty, arg)))
    end in
    let nexp = expand_exp_type exp gamma in
    (ncnd, nexp) :: expand_user_cases t gamma
  | (e1, e2) :: t -> expand_error ("expected user case: " ^ string_of_rexp_type e1 ^ " : " ^ string_of_rexp_type e2)
  | [] -> []

let rec expand_defs defs gamma =
  match defs with
  | RDefine (id, args, ret_type, body) :: t ->
    let new_args = map (fun a -> expand_user_type a gamma) args in
    let new_body = expand_exp_type body gamma in
    let new_def = RDefine (id, new_args, ret_type, new_body) in
    new_def :: expand_defs t gamma
  | RDefType (id, dt) :: t ->
    Hashtbl.add gamma id (None, dt);
    RDefType (id, dt) :: expand_defs t gamma
  | RTypeCons (id, side, dt) :: t ->
    Hashtbl.add gamma id (Some side, dt);
    RTypeCons (id, side, dt) :: expand_defs t gamma
  | [] -> []

let expand program =
  match program with
  | RProgram (dt, defs, e) ->
    let gamma = Hashtbl.create 10 in
    let new_defs = expand_defs defs gamma in
    let new_exps = expand_exp_type e gamma in
    RProgram (dt, new_defs, new_exps)
