open RProgram
open Gensym

exception UniquifyError of string

let uniquify_error s = raise (UniquifyError s)

let get_var_name v table : string =
  try
    let count = Hashtbl.find table v in
    v ^ (string_of_int count)
  with Not_found -> uniquify_error ("get_var_name: Variable " ^ v ^ " is undefined")

let uniquify_name v table : string =
  let cnt = Gensym.gen_int () in
  let _ = Hashtbl.replace table v cnt in
  v ^ (string_of_int cnt)

let rec uniquify_exp ast table : rexp =
  match ast with
  | RLet (v, i, b) ->
    let iexp = uniquify_exp_type i table in
    let uniq_var = uniquify_name v table in
    let bexp = uniquify_exp_type b table in
    RLet (uniq_var, iexp, bexp)
  | RUnOp (o, e) -> RUnOp (o, uniquify_exp_type e table)
  | RBinOp (o, l, r) -> RBinOp (o, uniquify_exp_type l table, uniquify_exp_type r table)
  | RVar v -> RVar (get_var_name v table)
  | RVector es -> RVector (List.map (fun e -> uniquify_exp_type e table) es)
  | RVectorRef (e, i) -> RVectorRef (uniquify_exp_type e table, i)
  | RVectorSet (v, i, e) -> RVectorSet (uniquify_exp_type v table, i, uniquify_exp_type e table)
  | RVectorLength v -> RVectorLength (uniquify_exp_type v table)
  | RAnd (l, r) -> RAnd (uniquify_exp_type l table, uniquify_exp_type r table)
  | ROr (l, r) -> ROr (uniquify_exp_type l table, uniquify_exp_type r table)
  | RNot e -> RNot (uniquify_exp_type e table)
  | RIf (cnd, thn, els) -> RIf (uniquify_exp_type cnd table, uniquify_exp_type thn table, uniquify_exp_type els table)
  | RCmp (o, l, r) -> RCmp (o, uniquify_exp_type l table, uniquify_exp_type r table)
  | RPrint e -> RPrint (uniquify_exp_type e table)
  | RWhile (c, e) -> RWhile (uniquify_exp_type c table, uniquify_exp_type e table)
  | _ -> ast

and uniquify_exp_type ast table : rexp_type =
  match ast with
  | TypeIs (dt, e) -> TypeIs (dt, uniquify_exp e table)

let uniquify ast : rprogram =
  match ast with
  | RProgram (_, defs, e) -> RProgram (None, defs, uniquify_exp_type e (Hashtbl.create 10))
