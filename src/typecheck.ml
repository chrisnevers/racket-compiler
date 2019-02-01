open RProgram
open List
open Helper

exception TypecheckError of string

let typecheck_error s = raise (TypecheckError s)

let rec subst_type a nty ty =
  let _rec ty = subst_type a nty ty in
  match ty with
  | TypeInt | TypeBool | TypeVoid | TypeChar -> ty
  | TypeVector ts -> TypeVector (map _rec ts)
  | TypeVar s -> if s = a then nty else ty
  | TypeFix t -> TypeFix (_rec t)
  | TypeForAll (s, t) -> if s = a then ty else TypeForAll (s, _rec t)
  | TypeArray t -> TypeArray (_rec t)
  | TypeFunction (args, ret) -> TypeFunction (map _rec args, _rec ret)
  | TypePlus (l, r) -> TypePlus (_rec l, _rec r)

let rec unfold_type ty =
  let _rec ty = unfold_type ty in
  match ty with
  | TypeFix (TypeForAll (a, ty')) -> subst_type a ty ty'
  | TypeVector ts -> TypeVector (map _rec ts)
  | TypeFix t -> TypeFix (_rec t)
  | TypeForAll (s, t) -> TypeForAll (s, _rec t)
  | TypeArray t -> TypeArray (_rec t)
  | TypeFunction (args, ret) -> TypeFunction (map _rec args, _rec ret)
  | TypePlus (l, r) -> TypePlus (_rec l, _rec r)
  | _ -> ty

let compare_type a b = unfold_type a = unfold_type b

let compare_args = (fun a b -> if not (compare_type a b) then
  typecheck_error ("args not the same type: " ^ (string_of_datatype a) ^ " - " ^ string_of_datatype b)
)

let rec add_args_to_gamma asc tbl =
  match asc with
  | (id, dt) :: t -> Hashtbl.replace tbl id (Some dt); add_args_to_gamma t tbl
  | [] -> ()

let get_value sigma gamma id =
  try Hashtbl.find sigma id, RVar id
  with Not_found -> try Hashtbl.find gamma id, RFunctionRef id
  with Not_found -> typecheck_error (id ^ " not found in sigma nor gamma")

let get_function_dt dt =
  match dt with
  | Some TypeFunction (args, ret) -> (args, ret)
  | _ -> typecheck_error ("expected function type: " ^ string_of_datatype_option dt)

let get_arity exp =
  match exp with
  | TypeIs (_, RArray (len, es)) -> Some len
  | TypeIs (_, RVector [TypeIs (_, RInt len); TypeIs (_, RArray _)]) -> Some len
  | _ -> None

(* Uses first element as expected datatype and arity of array *)
let typecheck_array_elements exps =
  let datatype = match hd exps with TypeIs (Some dt, e) -> dt in
  let arity = get_arity (hd exps) in
  let _ = iter (fun e ->
    if get_datatype e = datatype then
    if get_arity e = arity then ()
    else typecheck_error "all elements in array must have same arity"
    else typecheck_error "all elements in array must have same type"
  ) exps
  in datatype

let rec typecheck_exp exp table sigma =
  let _rec e = typecheck_exp_type e table sigma in
  match exp with
  | RInt i  -> make_tint (RInt i)
  | RChar c -> make_tchar (RChar c)
  | RBool b -> make_tbool (RBool b)
  | RVoid   -> make_tvoid RVoid
  | RArray (len, exps) ->
    let typed_exps = map _rec exps in
    (* Do not allow empty arrays *)
    if len < 1 then typecheck_error "array must have one element"
    else
      let datatype = typecheck_array_elements typed_exps in
      TypeIs (Some (TypeArray datatype), RArray (len, typed_exps))
  | RArraySet (v, i, e) ->
    (* Ensure array index is of type int *)
    let ni = _rec i in
    let idt = get_datatype ni in
    begin match idt with
    | TypeInt -> ()
    | _ -> typecheck_error "typecheck_exp: array index must be type int." end;
    (* Ensure new value's type matches array's type *)
    let nv = _rec v in
    let dt = get_datatype nv in (
    match dt with
    | TypeArray datatype ->
      let ne = _rec e in
      let edt = get_datatype ne in
      if (compare_type datatype edt) then
        make_tvoid (RArraySet (nv, ni, ne))
      else typecheck_error ("typecheck_exp: array-set! must operate on same type. Expected " ^ (string_of_datatype datatype) ^ " but received " ^ (string_of_datatype edt))
    | _ -> typecheck_error ("typecheck_exp: array-set! must operate on array. Received: " ^ (string_of_datatype dt)))
  | RArrayRef (v, i) ->
    let ni = _rec i in
    let idt = get_datatype ni in
    begin match idt with
    | TypeInt -> ()
    | _ -> typecheck_error "typecheck_exp: array index must be type int." end;
    let nv = _rec v in
    let dt = get_datatype nv in (
      match dt with
      | TypeArray datatype -> TypeIs (Some datatype, RArrayRef (nv, ni))
      | _ -> typecheck_error ("typecheck_exp: Array-ref must operate on array. Received: " ^ (string_of_datatype dt))
    )
  | RVector exps ->
    let typed_exps = map _rec exps in
    let datatypes = get_datatypes typed_exps in
    TypeIs (Some (TypeVector datatypes), RVector typed_exps)
  | RVectorRef (v, i) ->
    let nv = _rec v in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
          let ref_type = nth datatypes i in
          TypeIs (Some ref_type, RVectorRef (nv, i))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-ref must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorSet (v, i, e) ->
    let nv = _rec v in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
      let tk = nth datatypes i in
      let ne = _rec e in
      let edt = get_datatype ne in
      if (compare_type tk edt) then
        make_tvoid (RVectorSet (nv, i, ne))
      else typecheck_error ("typecheck_exp: vector-set! must operate on same type. Expected " ^ (string_of_datatype tk) ^ " but received " ^ (string_of_datatype edt))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-set! must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorLength v ->
    let nv = _rec v in
    let vdt = get_datatype nv in
    if is_vector vdt then make_tint (RInt (get_vector_length vdt))
    else typecheck_error ("typecheck_exp: Vector-length must operate on vector. Received: " ^ (string_of_datatype vdt))
  | RFunctionRef id | RVar id ->
    let dt, var = get_value table sigma id in
    TypeIs (dt, var)
  | RAnd (l, r) ->
    let nl = _rec l in
    let ldt = get_datatype nl in
    let nr = _rec r in
    let rdt = get_datatype nr in
    if (compare_type ldt TypeBool) && (compare_type rdt TypeBool) then
      make_tbool (RAnd (nl, nr))
    else typecheck_error "typecheck_exp: And expressions must operate on boolean values"
  | ROr (l, r) ->
    let nl = _rec l in
    let ldt = get_datatype nl in
    let nr = _rec r in
    let rdt = get_datatype nr in
    if (compare_type ldt TypeBool) && (compare_type rdt TypeBool) then
      make_tbool (ROr (nl, nr))
    else typecheck_error "typecheck_exp: Or expressions must operate on boolean values"
  | RNot e ->
    let ne = _rec e in
    let edt = get_datatype ne in
    if (compare_type edt TypeBool) then
      TypeIs (Some edt, RNot ne)
    else typecheck_error "typecheck_exp: Not expressions must operate on boolean values"
  | RIf (cnd, thn, els) ->
    let ncnd = _rec cnd in
    let cndt = get_datatype ncnd in
    let nthn = _rec thn in
    let thdt = get_datatype nthn in
    let nels = _rec els in
    let eldt = get_datatype nels in
    if not (compare_type cndt TypeBool) then
      typecheck_error "typecheck_exp: If condition must evaluate to boolean value"
    else if (compare_type thdt eldt) then
      TypeIs (Some thdt, RIf (ncnd, nthn, nels))
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same type"
  | RCmp (o, l, r) ->
    let nl = _rec l in
    let ldt = get_datatype nl in
    let nr = _rec r in
    let rdt = get_datatype nr in
    (match o with
    | ">" | ">=" | "<" | "<=" ->
      if (compare_type ldt TypeInt) && (compare_type rdt TypeInt) then make_tbool (RCmp (o, nl, nr))
      else typecheck_error ("typecheck_exp: " ^ o ^ " operates on integers")
    | "eq?" ->
      if (compare_type ldt rdt) then make_tbool (RCmp (o, nl, nr))
      else typecheck_error "typecheck_exp: eq? only compares same type"
    | _ -> typecheck_error "typecheck_exp: unexpected compare operator")
  | RUnOp ("-", e) ->
    let ne = _rec e in
    let edt = get_datatype ne in
    if (compare_type edt TypeInt) then make_tint (RUnOp ("-", ne))
    else typecheck_error ("typecheck_exp: - must be applied on integer")
  | RUnOp ("+", e) ->
    let ne = _rec e in
    let edt = get_datatype ne in
    if (compare_type edt TypeInt) then ne
    else typecheck_error ("typecheck_exp: + must be applied on integer")
  | RUnOp (o, e) -> typecheck_error ("typecheck_exp: " ^ o ^ " not a unary operator")
  | RBinOp (o, l, r) ->
    let nl = _rec l in
    let ldt = get_datatype nl in
    let nr = _rec r in
    let rdt = get_datatype nr in
    if (compare_type ldt TypeInt) && (compare_type rdt TypeInt) then make_tint (RBinOp (o, nl, nr))
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integers")
  | RLet (v, i, b) ->
    let ni = _rec i in
    let idt = get_datatype_option ni in
    let _ = Hashtbl.add table v idt in
    let nb = _rec b in
    let bdt = get_datatype_option nb in
    TypeIs (bdt, RLet (v, ni, nb))
  | RRead -> make_tint (RRead)
  | RPrint e ->
    let ne = _rec e in
    make_tvoid (RPrint ne)
  | RWhile (c, e) ->
    let nc = _rec c in
    let ne = _rec e in
    let dt = get_datatype_option ne in
    TypeIs (dt, RWhile (nc, ne))
  | RLambda (args, ret, e) ->
    add_args_to_gamma args table;
    let ne = _rec e in
    let dt = get_datatype ne in
    let ldt = TypeFunction (map (fun (id, dt) -> dt) args, ret) in
    if unfold_type dt <> unfold_type ret then typecheck_error ("lambda return type does not match body: "
      ^ string_of_datatype dt ^ " : " ^ string_of_datatype ret)
    else TypeIs (Some ldt, RLambda (args, ret, ne))
  | RApply (func, args) ->
    let nid = _rec func in
    let fdt = get_datatype_option nid in
    let (fun_args, fun_ret) = get_function_dt fdt in
    let new_args = map _rec args in
    (try
      iter2 compare_args (map get_datatype new_args) fun_args;
      TypeIs (Some fun_ret, RApply (nid, new_args))
    with Invalid_argument _ ->
      print_endline (string_of_int (length new_args));
      print_endline (string_of_int (length fun_args));
      typecheck_error "function arguments do not match parameters"
    )
  | RInl (e, dt) ->
    let ne = _rec e in
    let ety = get_datatype ne in
    TypeIs (Some (TypePlus(ety, dt)), RInl (ne, dt))
  | RInr (dt, e) ->
    let ne = _rec e in
    let ety = get_datatype ne in
    TypeIs (Some (TypePlus(dt, ety)), RInr (dt, ne))
  | RCase (e, cases) ->
    let ne = _rec e in
    let dt = get_datatype ne in
    let ncases = map (fun (c, b) ->
      let id, ty = begin match c with
        | TypeIs (_, RInl (TypeIs (dt, RVar id), _)) -> (id, dt)
        | TypeIs (_, RInr (_, TypeIs (dt, RVar id))) -> (id, dt)
        end
      in
      let _ = Hashtbl.add table id ty in
      let nc = _rec c in
      let nb = _rec b in
      let ncdt = get_datatype nc in
      (nc, nb)
    ) cases in
    let cnd_dts, body_dts = split ncases in
    let valid_cnd = for_all (fun c -> get_datatype c = dt) cnd_dts in
    let expected_ret = get_datatype (hd body_dts) in
    let valid_bod = for_all (fun b -> get_datatype b = expected_ret) body_dts in
    if valid_cnd = false then typecheck_error "match case is not of correct type" else
    if valid_bod = false then typecheck_error "match cases do not return same type" else
    TypeIs (Some expected_ret, RCase (ne, ncases))
  | RFold e -> _rec e
  | RUnfold e ->
    let TypeIs (Some dt, ne) = _rec e in
    TypeIs (Some (unfold_type dt), ne)
  | RTyLambda (ty, e) ->
    let ne = _rec e in
    let dt = get_datatype ne in
    TypeIs (Some (TypeForAll (ty, dt)), RTyLambda (ty, ne))
  | RBegin _ -> typecheck_error "should not have begin in typecheck"
  | RWhen (_, _) -> typecheck_error "should not have when in typecheck"
  | RUnless (_, _) -> typecheck_error "should not have unless in typecheck"
  | RCollect _ -> typecheck_error "should not have collect in typecheck"
  | RAllocate (_, _) -> typecheck_error "should not have allocate in typecheck"
  | RGlobalValue _ -> typecheck_error "should not have globalvalue in typecheck"
  | _ -> typecheck_error (string_of_rexp exp)

and typecheck_exp_type exp table sigma =
  match exp with
  | TypeIs (None, e) -> typecheck_exp e table sigma
  | TypeIs (dt, RFold (TypeIs (_, e))) -> TypeIs (dt, e)
  | TypeIs (dt, e) -> TypeIs (dt, e)

let rec typecheck_defs defs sigma =
  match defs with
  | [] -> []
  | RDefine (id, args, ret_type, body) :: t ->
    let gamma = Hashtbl.create 10 in
    add_args_to_gamma args gamma;
    let _, arg_types = split args in
    Hashtbl.replace sigma id (Some (TypeFunction (arg_types, ret_type)));
    let next_defs = typecheck_defs t sigma in
    let new_body = typecheck_exp_type body gamma sigma in
    let body_ret_type = get_datatype new_body in
    if body_ret_type = ret_type then
    (* if unfold_type body_ret_type = unfold_type ret_type then *)
      RDefine (id, args, ret_type, new_body) :: next_defs
    else typecheck_error ("Typecheck Error: function " ^ id ^
      " has a different return type (" ^ string_of_datatype ret_type
      ^ ") than its body (" ^ string_of_datatype body_ret_type ^ ")")
  | d :: t -> d :: typecheck_defs t sigma


let typecheck program : rprogram =
  match program with
  | RProgram (_, defs, e) ->
    let sigma = Hashtbl.create 10 in
    let ndefs = typecheck_defs defs sigma in
    let ne = typecheck_exp_type e (Hashtbl.create 10) sigma in
    match ne with
    | TypeIs (dt, _) -> RProgram (dt, ndefs, ne)
