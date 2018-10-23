open RProgram
open List
open Helper

exception TypecheckError of string

let typecheck_error s = raise (TypecheckError s)

let compare_args = (fun a b -> if get_datatype a <> b then
  typecheck_error ("args not the same type: " ^ (string_of_datatype (get_datatype a)) ^ " - " ^ string_of_datatype b)
)

let rec add_to_table asc tbl =
  match asc with
  | (id, dt) :: t -> Hashtbl.replace tbl id (Some dt); add_to_table t tbl
  | [] -> ()

let get_value sigma gamma id =
  try Hashtbl.find sigma id, RVar id
  with Not_found -> try Hashtbl.find gamma id, RFunctionRef id
  with Not_found -> typecheck_error (id ^ " not found in sigma nor gamma")

let get_some_func_types dt =
  match dt with
  | Some TypeFunction (args, ret) -> (args, ret)
  | _ -> typecheck_error ("expected function type: " ^ string_of_datatype_option dt)

let rec typecheck_exp exp table sigma =
  match exp with
  | RInt i  -> make_tint (RInt i)
  | RBool b -> make_tbool (RBool b)
  | RVoid   -> make_tvoid RVoid
  | RVector exps ->
    let typed_exps = List.map (fun t -> typecheck_exp_type t table sigma) exps in
    let datatypes = get_datatypes typed_exps in
    TypeIs (Some (TypeVector datatypes), RVector typed_exps)
  | RVectorRef (v, i) ->
    let nv = typecheck_exp_type v table sigma in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
          let ref_type = List.nth datatypes i in
          TypeIs (Some ref_type, RVectorRef (nv, i))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-ref must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorSet (v, i, e) ->
    let nv = typecheck_exp_type v table sigma in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
      let tk = List.nth datatypes i in
      let ne = typecheck_exp_type e table sigma in
      let edt = get_datatype ne in
      if tk = edt then
        make_tvoid (RVectorSet (nv, i, ne))
      else typecheck_error ("typecheck_exp: vector-set! must operate on same type. Expected " ^ (string_of_datatype tk) ^ " but received " ^ (string_of_datatype edt))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-set! must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorLength v ->
    let nv = typecheck_exp_type v table sigma in
    let vdt = get_datatype nv in
    if is_vector vdt then make_tint (RInt (get_vector_length vdt))
    else typecheck_error ("typecheck_exp: Vector-length must operate on vector. Received: " ^ (string_of_datatype vdt))
  | RFunctionRef id | RVar id ->
    let dt, var = get_value table sigma id in
    TypeIs (dt, var)
  | RAnd (l, r) ->
    let nl = typecheck_exp_type l table sigma in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table sigma in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeBool && rdt = Some TypeBool then
      make_tbool (RAnd (nl, nr))
    else typecheck_error "typecheck_exp: And expressions must operate on boolean values"
  | ROr (l, r) ->
    let nl = typecheck_exp_type l table sigma in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table sigma in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeBool && rdt = Some TypeBool then
      make_tbool (ROr (nl, nr))
    else typecheck_error "typecheck_exp: Or expressions must operate on boolean values"
  | RNot e ->
    let ne = typecheck_exp_type e table sigma in
    let edt = get_datatype_option ne in
    if edt = Some TypeBool then
      TypeIs (edt, RNot ne)
    else typecheck_error "typecheck_exp: Not expressions must operate on boolean values"
  | RIf (cnd, thn, els) ->
    let ncnd = typecheck_exp_type cnd table sigma in
    let cndt = get_datatype_option ncnd in
    let nthn = typecheck_exp_type thn table sigma in
    let thdt = get_datatype_option nthn in
    let nels = typecheck_exp_type els table sigma in
    let eldt = get_datatype_option nels in
    if cndt <> Some TypeBool then
      typecheck_error "typecheck_exp: If condition must evaluate to boolean value"
    else if thdt = eldt then
      TypeIs (thdt, RIf (ncnd, nthn, nels))
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same type"
  | RCmp (o, l, r) ->
    let nl = typecheck_exp_type l table sigma in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table sigma in
    let rdt = get_datatype_option nr in
    (match o with
    | ">" | ">=" | "<" | "<=" ->
      if ldt = Some TypeInt && rdt = Some TypeInt then make_tbool (RCmp (o, nl, nr))
      else typecheck_error ("typecheck_exp: " ^ o ^ " operates on integers")
    | "eq?" ->
      if ldt = rdt then make_tbool (RCmp (o, nl, nr))
      else typecheck_error "typecheck_exp: eq? only compares same type"
    | _ -> typecheck_error "typecheck_exp: unexpected compare operator")
  | RUnOp ("-", e) ->
    let ne = typecheck_exp_type e table sigma in
    let edt = get_datatype_option ne in
    if edt = Some TypeInt then make_tint (RUnOp ("-", ne))
    else typecheck_error ("typecheck_exp: - must be applied on integer")
  | RUnOp ("+", e) ->
    let ne = typecheck_exp_type e table sigma in
    let edt = get_datatype_option ne in
    if edt = Some TypeInt then ne
    else typecheck_error ("typecheck_exp: + must be applied on integer")
  | RUnOp (o, e) -> typecheck_error ("typecheck_exp: " ^ o ^ " not a unary operator")
  | RBinOp (o, l, r) ->
    let nl = typecheck_exp_type l table sigma in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table sigma in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeInt && rdt = Some TypeInt then make_tint (RBinOp (o, nl, nr))
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integers")
  | RLet (v, i, b) ->
    let ni = typecheck_exp_type i table sigma in
    let idt = get_datatype_option ni in
    let _ = Hashtbl.add table v idt in
    let nb = typecheck_exp_type b table sigma in
    let bdt = get_datatype_option nb in
    TypeIs (bdt, RLet (v, ni, nb))
  | RRead -> make_tint (RRead)
  | RPrint e ->
    let ne = typecheck_exp_type e table sigma in
    make_tvoid (RPrint ne)
  | RWhile (c, e) ->
    let nc = typecheck_exp_type c table sigma in
    let ne = typecheck_exp_type e table sigma in
    let dt = get_datatype_option ne in
    TypeIs (dt, RWhile (nc, ne))
  | RLambda (args, ret, e) ->
    add_to_table args table;
    let ne = typecheck_exp_type e table sigma in
    let dt = get_datatype ne in
    let ldt = TypeFunction (map (fun (id, dt) -> dt) args, ret) in
    if dt <> ret then typecheck_error "lambda return type does not match body"
    else TypeIs (Some ldt, RLambda (args, ret, ne))
  | RApply (func, args) ->
    let nid = typecheck_exp_type func table sigma in
    let fdt = get_datatype_option nid in
    let (fun_args, fun_ret) = get_some_func_types fdt in
    let new_args = map (fun e -> typecheck_exp_type e table sigma) args in
    iter2 compare_args new_args fun_args;
    TypeIs (Some fun_ret, RApply (nid, new_args))
  | RBegin _ -> typecheck_error "should not have begin in typecheck"
  | RWhen (_, _) -> typecheck_error "should not have when in typecheck"
  | RUnless (_, _) -> typecheck_error "should not have unless in typecheck"
  | RCollect _ -> typecheck_error "should not have collect in typecheck"
  | RAllocate (_, _) -> typecheck_error "should not have allocate in typecheck"
  | RGlobalValue _ -> typecheck_error "should not have globalvalue in typecheck"

and typecheck_exp_type exp table sigma =
  match exp with
  | TypeIs (None, e) -> typecheck_exp e table sigma
  | TypeIs (dt, e) -> TypeIs (dt, e)

let rec typecheck_defs defs sigma =
  match defs with
  | [] -> []
  | RDefine (id, args, ret_type, body) :: t ->
    let gamma = Hashtbl.create 10 in
    add_to_table args gamma;
    let _, arg_types = List.split args in
    Hashtbl.replace sigma id (Some (TypeFunction (arg_types, ret_type)));
    let next_defs = typecheck_defs t sigma in
    let new_body = typecheck_exp_type body gamma sigma in
    let body_ret_type = get_datatype new_body in
    if body_ret_type = ret_type then
      RDefine (id, args, ret_type, new_body) :: next_defs
    else typecheck_error ("Typecheck Error: function " ^ id ^
      " has a different return type (" ^ string_of_datatype ret_type
      ^ ") than its body (" ^ string_of_datatype body_ret_type ^ ")")

let typecheck program : rprogram =
  match program with
  | RProgram (_, defs, e) ->
    let sigma = Hashtbl.create 10 in
    let ndefs = typecheck_defs defs sigma in
    let ne = typecheck_exp_type e (Hashtbl.create 10) sigma in
    match ne with
    | TypeIs (dt, _) -> RProgram (dt, ndefs, ne)
