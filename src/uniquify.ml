open RProgram
open Gensym
open List

exception UniquifyError of string

let uniquify_error s = raise (UniquifyError s)

let get_var_name v table sigma : string =
  try
    let count = Hashtbl.find table v in
    v ^ (string_of_int count)
  with Not_found ->
    try let count = Hashtbl.find sigma v in
    v ^ (string_of_int count)
    with Not_found -> uniquify_error ("get_var_name: Variable " ^ v ^ " is undefined")

let uniquify_name v table : string =
  let cnt = Gensym.gen_int () in
  let _ = Hashtbl.replace table v cnt in
  v ^ (string_of_int cnt)

let rec uniquify_exp ast table sigma : rexp =
  match ast with
  | RLet (v, i, b) ->
    let iexp = uniquify_exp_type i table sigma in
    let uniq_var = uniquify_name v table in
    let bexp = uniquify_exp_type b table sigma in
    RLet (uniq_var, iexp, bexp)
  | RUnOp (o, e) -> RUnOp (o, uniquify_exp_type e table sigma)
  | RBinOp (o, l, r) -> RBinOp (o, uniquify_exp_type l table sigma, uniquify_exp_type r table sigma)
  | RVar v -> RVar (get_var_name v table sigma)
  | RVector es -> RVector (List.map (fun e -> uniquify_exp_type e table sigma) es)
  | RVectorRef (e, i) -> RVectorRef (uniquify_exp_type e table sigma, i)
  | RVectorSet (v, i, e) -> RVectorSet (uniquify_exp_type v table sigma, i, uniquify_exp_type e table sigma)
  | RVectorLength v -> RVectorLength (uniquify_exp_type v table sigma)
  | RAnd (l, r) -> RAnd (uniquify_exp_type l table sigma, uniquify_exp_type r table sigma)
  | ROr (l, r) -> ROr (uniquify_exp_type l table sigma, uniquify_exp_type r table sigma)
  | RNot e -> RNot (uniquify_exp_type e table sigma)
  | RIf (cnd, thn, els) -> RIf (uniquify_exp_type cnd table sigma, uniquify_exp_type thn table sigma, uniquify_exp_type els table sigma)
  | RCmp (o, l, r) -> RCmp (o, uniquify_exp_type l table sigma, uniquify_exp_type r table sigma)
  | RPrint e -> RPrint (uniquify_exp_type e table sigma)
  | RWhile (c, e) -> RWhile (uniquify_exp_type c table sigma, uniquify_exp_type e table sigma)
  | RApply (id, args) -> RApply (uniquify_exp_type id table sigma, List.map (fun a -> uniquify_exp_type a table sigma) args)
  | RLambda (args, ret, e) ->
    let ids, dts = split args in
    let new_ids = map (fun id -> uniquify_name id table) ids in
    let new_args = combine new_ids dts in
    RLambda (new_args, ret, uniquify_exp_type e table sigma)
  | _ -> ast

and uniquify_exp_type ast table sigma : rexp_type =
  match ast with
  | TypeIs (dt, e) -> TypeIs (dt, uniquify_exp e table sigma)

let rec uniquify_defs defs sigma =
  match defs with
  | RDefine (id, args, ret_type, body) :: t ->
    let new_id = uniquify_name id sigma in
    let table = Hashtbl.create 10 in
    let arg_ids, arg_datatypes = List.split args in
    let new_arg_ids = List.map (fun a -> uniquify_name a table) arg_ids in
    let new_args = List.combine new_arg_ids arg_datatypes in
    (* Effectively adds all function prototypes before processing bodies *)
    let next_defs = uniquify_defs t sigma in
    let new_def = RDefine (new_id, new_args, ret_type, uniquify_exp_type body table sigma) in
    new_def :: next_defs
  | [] -> []

let uniquify ast : rprogram =
  match ast with
  | RProgram (_, defs, e) ->
    (* Stores function ids *)
    let sigma = (Hashtbl.create 10) in
    let new_defs = uniquify_defs defs sigma in
    let new_exp = uniquify_exp_type e (Hashtbl.create 10) sigma in
    RProgram (None, new_defs, new_exp)
