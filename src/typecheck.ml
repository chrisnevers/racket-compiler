open RProgram

exception TypecheckError of string

let typecheck_error s = raise (TypecheckError s)

let get_var_type v table =
  try Hashtbl.find table v
  with Not_found -> typecheck_error "get_var_type: Undeclared variable"

let rec typecheck_exp exp table : datatype =
  match exp with
  | RInt i -> TypeInt
  | RBool b -> TypeBool
  | RVar v -> get_var_type v table
  | RAnd (l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    if ltype = TypeBool && rtype = TypeBool then TypeBool
    else typecheck_error "typecheck_exp: And expressions must operate on boolean values"
  | ROr (l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    if ltype = TypeBool && rtype = TypeBool then TypeBool
    else typecheck_error "typecheck_exp: Or expressions must operate on boolean values"
  | RNot e ->
    let etype = typecheck_exp e table in
    if etype = TypeBool then TypeBool
    else typecheck_error "typecheck_exp: Not expressions must operate on boolean values"
  | RIf (cnd, thn, els) ->
    let ctype = typecheck_exp cnd table in
    let ttype = typecheck_exp thn table in
    let etype = typecheck_exp els table in
    if ctype != TypeBool then typecheck_error "typecheck_exp: If condition must evaluate to boolean value"
    else if ttype = etype then etype
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same type"
  | RCmp (o, l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    (match o with
    | ">" | ">=" | "<" | "<=" ->
      if ltype = TypeInt && rtype = TypeInt then TypeBool
      else typecheck_error ("typecheck_exp: " ^ o ^ " operates on integers")
    | "eq?" ->
      if ltype = rtype then TypeBool
      else typecheck_error "typecheck_exp: eq? only compares same type"
    | _ -> typecheck_error "typecheck_exp: unexpected compare operator")
  | RUnOp (o, e) ->
    let etype = typecheck_exp e table in
    if etype = TypeInt then TypeInt
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integer")
  | RBinOp (o, l, r) ->
    let ltype = typecheck_exp l table in
    let rtype = typecheck_exp r table in
    if ltype = TypeInt && rtype = TypeInt then TypeInt
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integers")
  | RLet (v, i, b) ->
    let itype = typecheck_exp i table in
    let _ = Hashtbl.add table v itype in
    let btype = typecheck_exp b table in
    btype
  | RRead -> TypeInt

let typecheck program : rprogram =
  match program with
  | RProgram (_, e) -> RProgram (typecheck_exp e (Hashtbl.create 10), e)
