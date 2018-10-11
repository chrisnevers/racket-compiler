open RProgram
open List
open Helper

exception TypecheckError of string

let typecheck_error s = raise (TypecheckError s)

let get_var_type v table =
  try Hashtbl.find table v
  with Not_found -> typecheck_error "get_var_type: Undeclared variable"

let rec typecheck_exp exp table =
  match exp with
  | RInt i  -> make_tint (RInt i)
  | RBool b -> make_tbool (RBool b)
  | RVoid   -> make_tvoid RVoid
  | RVector exps ->
    let typed_exps = List.map (fun t -> typecheck_exp_type t table) exps in
    let datatypes = get_datatypes typed_exps in
    TypeIs (Some (TypeVector datatypes), RVector typed_exps)
  | RVectorRef (v, i) ->
    let nv = typecheck_exp_type v table in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
          let ref_type = List.nth datatypes i in
          TypeIs (Some ref_type, RVectorRef (nv, i))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-ref must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorSet (v, i, e) ->
    let nv = typecheck_exp_type v table in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
      let tk = List.nth datatypes i in
      let ne = typecheck_exp_type e table in
      let edt = get_datatype ne in
      if tk = edt then
        make_tvoid (RVectorSet (nv, i, ne))
      else typecheck_error ("typecheck_exp: vector-set! must operate on same type. Expected " ^ (string_of_datatype tk) ^ " but received " ^ (string_of_datatype edt))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-set! must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorLength v ->
    let nv = typecheck_exp_type v table in
    let vdt = get_datatype nv in
    if is_vector vdt then make_tint (RInt (get_vector_length vdt))
    else typecheck_error ("typecheck_exp: Vector-length must operate on vector. Received: " ^ (string_of_datatype vdt))
  | RVar v -> TypeIs (get_var_type v table, RVar v)
  | RAnd (l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeBool && rdt = Some TypeBool then
      make_tbool (RAnd (nl, nr))
    else typecheck_error "typecheck_exp: And expressions must operate on boolean values"
  | ROr (l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeBool && rdt = Some TypeBool then
      make_tbool (ROr (nl, nr))
    else typecheck_error "typecheck_exp: Or expressions must operate on boolean values"
  | RNot e ->
    let ne = typecheck_exp_type e table in
    let edt = get_datatype_option ne in
    if edt = Some TypeBool then
      TypeIs (edt, RNot ne)
    else typecheck_error "typecheck_exp: Not expressions must operate on boolean values"
  | RIf (cnd, thn, els) ->
    let ncnd = typecheck_exp_type cnd table in
    let cndt = get_datatype_option ncnd in
    let nthn = typecheck_exp_type thn table in
    let thdt = get_datatype_option nthn in
    let nels = typecheck_exp_type els table in
    let eldt = get_datatype_option nels in
    if cndt <> Some TypeBool then
      typecheck_error "typecheck_exp: If condition must evaluate to boolean value"
    else if thdt = eldt then
      TypeIs (thdt, RIf (ncnd, nthn, nels))
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same type"
  | RCmp (o, l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    (match o with
    | ">" | ">=" | "<" | "<=" ->
      if ldt = Some TypeInt && ldt = Some TypeInt then make_tbool (RCmp (o, nl, nr))
      else typecheck_error ("typecheck_exp: " ^ o ^ " operates on integers")
    | "eq?" ->
      if ldt = rdt then make_tbool (RCmp (o, nl, nr))
      else typecheck_error "typecheck_exp: eq? only compares same type"
    | _ -> typecheck_error "typecheck_exp: unexpected compare operator")
  | RUnOp (o, e) ->
    let ne = typecheck_exp_type e table in
    let edt = get_datatype_option ne in
    if edt = Some TypeInt then make_tint (RUnOp (o, ne))
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integer")
  | RBinOp (o, l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeInt && rdt = Some TypeInt then make_tint (RBinOp (o, nl, nr))
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integers")
  | RLet (v, i, b) ->
    let ni = typecheck_exp_type i table in
    let idt = get_datatype_option ni in
    let _ = Hashtbl.add table v idt in
    let nb = typecheck_exp_type b table in
    let bdt = get_datatype_option nb in
    TypeIs (bdt, RLet (v, ni, nb))
  | RRead -> make_tint (RRead)
  | RPrint e ->
    let ne = typecheck_exp_type e table in
    make_tvoid (RPrint ne)
  | RWhile (c, e) ->
    let nc = typecheck_exp_type c table in
    let ne = typecheck_exp_type e table in
    let dt = get_datatype_option ne in
    TypeIs (dt, RWhile (nc, ne))
  | RBegin _ -> typecheck_error "should not have begin in typecheck"
  | RWhen (_, _) -> typecheck_error "should not have when in typecheck"
  | RUnless (_, _) -> typecheck_error "should not have unless in typecheck"
  | RCollect _ -> typecheck_error "should not have collect in typecheck"
  | RAllocate (_, _) -> typecheck_error "should not have allocate in typecheck"
  | RGlobalValue _ -> typecheck_error "should not have globalvalue in typecheck"

and typecheck_exp_type exp table =
  match exp with
  | TypeIs (_, e) -> typecheck_exp e table

let typecheck program : rprogram =
  match program with
  | RProgram (_, defs, e) ->
    let ne = typecheck_exp_type e (Hashtbl.create 10) in
    match ne with
    | TypeIs (dt, _) -> RProgram (dt, defs, ne)
