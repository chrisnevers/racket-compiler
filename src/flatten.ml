open CProgram
open RProgram
open Helper
open Gensym
open List

exception FlattenError of string

let flatten_error s = raise (FlattenError s)

let get_id f =
  match f with
  | CFunctionRef label -> label
  | CVar label -> label
  | _ -> flatten_error ("Expected function-ref: " ^ string_of_carg f)

let get_var_name v name =
  match v with
  | Some name -> name
  | None -> Gensym.gen_str name

let get_carg_of_rarg a : carg =
  match a with
  | RBool b -> CBool b
  | RInt i -> CInt i
  | RVar name -> CVar name
  | RVoid -> CVoid
  | RGlobalValue label -> CGlobalValue label
  | RFunctionRef label -> CFunctionRef label
  | _ -> flatten_error ("get_carg_of_rarg: Expected to receive CArg but received " ^ (string_of_rexp a))

let get_ccmp_of_rcmp o : ccmp =
  match o with
  | "eq?" -> CEq
  | "<" -> CL
  | "<=" -> CLE
  | ">" -> CG
  | ">=" -> CGE
  | _ -> flatten_error ("get_ccmp_of_rcmp: Expected compare operator but received " ^ o)

let flatten_arg ?(v=None) a : carg * cstmt list * (string * datatype) list =
  let flat_arg = get_carg_of_rarg a in
  let stmts = if v = None then [] else [CAssign (get_var_name v (string_of_carg_type flat_arg), CArg flat_arg)] in
  let var_list = [] in
  (flat_arg, stmts, var_list)

(* let rec flatten_exp ?(v=None) e : carg * cstmt list * (string * datatype) list = *)

let rec flatten_typed_exp ?(v=None) exp =
  match exp with
  | TypeIs (None, ue) -> flatten_error ("flatten: expression is untyped: " ^ (string_of_rexp ue))
  | TypeIs (Some dt, e) -> (
    match e with
    | RVar _ | RInt _ | RBool _ | RVoid | RGlobalValue _ | RFunctionRef _ ->
      flatten_arg e ~v:v
    | RAnd (l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "and" in
      let flat_arg = CVar var_name in
      let lif_cnd = CCmp (CEq, CBool true, larg) in
      let rif_cnd = CCmp (CEq, CBool true, rarg) in
      (* We only execute this if first condition is true, so if this condition is true, then set var to true, otherwise false *)
      let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
      (* Execute first condition, if true continue to next condition (execute its stmts then call if to see if true), otherwise short circuit to false *)
      let lif = CIf (lif_cnd, rstmts @ [rif], [CAssign (var_name, CArg (CBool false))]) in
      (* Execute lstmts see if left is true *)
      let stmts = lstmts @ [lif] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | ROr (l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "or" in
      let flat_arg = CVar var_name in
      let lif_cnd = CCmp (CEq, CBool true, larg) in
      let rif_cnd = CCmp (CEq, CBool true, rarg) in
      (* We only execute this if first condition is false, so if this condition is true, then set var to true, otherwise false *)
      let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
      (* Execute first condition, if true then set var to true, else see if next condition is true *)
      let lif = CIf (lif_cnd, [CAssign (var_name, CArg (CBool true))], rstmts @ [rif]) in
      (* Execute lstmts see if left is true *)
      let stmts = lstmts @ [lif] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | RNot e ->
      let (earg, estmts, evars) = flatten_typed_exp e in
      let var_name = get_var_name v "not" in
      let flat_arg = CVar var_name in
      let stmts = estmts @ [CAssign (var_name, CNot earg)] in
      let var_list = if v = None then (var_name, dt) :: evars else evars in
      (flat_arg, stmts, var_list)
    | RIf (cnd, thn, els) ->
      let var_name = get_var_name v "if" in
      let (cnd_arg, cnd_stmts, cnd_vars) = flatten_typed_exp cnd in
      (* Assign result of then and else conditions to lhs variable / or newly created tmp *)
      let (thn_arg, thn_stmts, thn_vars) = flatten_typed_exp thn ~v:(Some var_name) in
      let (els_arg, els_stmts, els_vars) = flatten_typed_exp els ~v:(Some var_name) in
      let if_cnd = CCmp (CEq, CBool true, cnd_arg) in
      let flat_arg = CVar var_name in
      let stmts = cnd_stmts @ [CIf (if_cnd, thn_stmts, els_stmts)] in
      let var_list = if v = None then (var_name, dt) :: cnd_vars @ thn_vars @ els_vars else cnd_vars @ thn_vars @ els_vars in
      (flat_arg, stmts, var_list)
    | RWhile (cnd, thn) ->
      let var_name = get_var_name v "while" in
      let (cnd_arg, cnd_stmts, cnd_vars) = flatten_typed_exp cnd in
      let (thn_arg, thn_stmts, thn_vars) = flatten_typed_exp thn ~v:(Some var_name) in
      let while_cnd = CCmp (CEq, CBool true, cnd_arg) in
      let flat_arg = CVar var_name in
      let stmts = [CAssign (var_name, CArg (CVoid)); CWhile (cnd_stmts, while_cnd, thn_stmts)] in
      let var_list = if v = None then (var_name, dt) :: cnd_vars @ thn_vars else cnd_vars @ thn_vars in
      (flat_arg, stmts, var_list)
    | RCmp (o, l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "cmp" in
      let flat_arg = CVar var_name in
      let ccmp = get_ccmp_of_rcmp o in
      let stmts = lstmts @ rstmts @ [CAssign (var_name, CCmp (ccmp, larg, rarg))] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | RUnOp (o, e) ->
      let (earg, estmts, evars) = flatten_typed_exp e in
      let var_name = get_var_name v "unop" in
      let flat_arg = CVar var_name in
      let stmts = estmts @ [CAssign (var_name, CUnOp (o, earg))] in
      let var_list = if v = None then (var_name, dt) :: evars else evars in
      (flat_arg, stmts, var_list)
    | RBinOp (o, l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "binop" in
      let flat_arg = CVar var_name in
      let stmts = lstmts @ rstmts @ [CAssign (var_name, CBinOp (o, larg, rarg))] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | RLet (name, i, b) ->
      (* Assign result of inner expression to the variable being declared *)
      let (iarg, istmts, ivars) = flatten_typed_exp i ~v:(Some name) in
      (* Assign result of body function to whatever variable this expression is a child of *)
      let (barg, bstmts, bvars) = flatten_typed_exp b ~v:v in
      let flat_arg = barg in
      let stmts = istmts @ bstmts in
      let var_list = (name, get_datatype i) :: ivars @ bvars in
      (flat_arg, stmts, var_list)
    | RRead ->
      let var_name = get_var_name v "read" in
      let flat_arg = CVar var_name in
      let stmts = [CAssign (var_name, CRead)] in
      let var_list = if v = None then [(var_name, dt)] else [] in
      (flat_arg, stmts, var_list)
    | RPrint e ->
      let (earg, estmts, evars) = flatten_typed_exp e in
      let edt = get_datatype e in
      let var_name = get_var_name v "print" in
      let flat_arg = CVar var_name in
      (* Revisit assign to void? *)
      let stmts = estmts @ [CAssign (var_name, CPrint (edt, earg))] in
      let var_list = if v = None then (var_name, dt) :: evars else evars in
      (flat_arg, stmts, var_list)
    | RArraySet (arr, i, e) ->
      let (ararg, arstmts, arvars) = flatten_typed_exp arr in
      let (iarg, istmts, ivars) = flatten_typed_exp i in
      let (earg, estmts, evars) = flatten_typed_exp e in
      let flat_arg = CVoid in
      let stmts = arstmts @ istmts @ estmts @ [CArraySet (ararg, iarg, earg)] in
      let var_list = arvars @ ivars @ evars in
      (flat_arg, stmts, var_list)
    | RVectorSet (vec, i, e) ->
      let (varg, vstmts, vvars) = flatten_typed_exp vec in
      let (earg, estmts, evars) = flatten_typed_exp e in
      let flat_arg = CVoid in
      let stmts = vstmts @ estmts @ [CVectorSet (varg, i, earg)] in
      let var_list = vvars @ evars in
      (flat_arg, stmts, var_list)
    | RVectorRef (ve, i) ->
      let var_name = get_var_name v "vref" in
      let (varg, vstmts, vvars) = flatten_typed_exp ve in
      let flat_arg = CVar var_name in
      let stmts = vstmts @ [CAssign (var_name, CVectorRef (varg, i))] in
      let var_list = if v = None then (var_name, dt) :: vvars else vvars in
      (flat_arg, stmts, var_list)
    | RCollect i -> (CVoid, [CCollect i], [])
    | RAllocate (i, ty) ->
      let var_name = get_var_name v "alloc" in
      let flat_arg = CVar var_name in
      let stmts = [CAssign (var_name, CAlloc (i, ty))] in
      let var_list = if v = None then [(var_name, dt)] else [] in
      (flat_arg, stmts, var_list)
    | RApply (fun_id, args) ->
      let flat_args, flat_stmts, flat_vars =
        split3 (map (fun a -> flatten_typed_exp a) args) in
      let fun_arg, fun_stmts, fun_vars = flatten_typed_exp fun_id in
      let id = get_id fun_arg in
      let var_name = get_var_name v id in
      let flat_arg = CVar var_name in
      let apply = CApply (fun_arg, flat_args) in
      let stmts = concat flat_stmts @ fun_stmts @ [CAssign (var_name, apply)] in
      let var_list = if v = None then (var_name, dt) :: concat flat_vars @ fun_vars else concat flat_vars @ fun_vars in
      (flat_arg, stmts, var_list)
    (* Invalid expressions *)
    | RLambda _ -> flatten_error "should not have lambda in vector"
    | RArray _ -> flatten_error "should not have array in flatten"
    | RVector _ -> flatten_error "should not have vector in flatten"
    | RVectorLength _ -> flatten_error "should not have vector-length in flatten"
    | RBegin _ -> flatten_error "should not have begin in flatten"
    | RWhen (_, _) -> flatten_error "should not have when in flatten"
    | RUnless (_, _) -> flatten_error "should not have unless in flatten"
  )

let rec flatten_defs defs =
  match defs with
  | RDefine (id, args, ret, body) :: t ->
    let (arg, stmts, vars) = flatten_typed_exp body in
    let var2dt = make_hashtable (vars @ args) in
    CDefine (id, args, ret, var2dt, stmts @ [CReturn arg]) :: flatten_defs t
  | [] -> []

let flatten program : cprogram =
  match program with
  | RProgram (Some dt, defs, e) ->
    let (arg, stmts, vars) = flatten_typed_exp e in
    let new_defs = flatten_defs defs in
    let new_stmts = stmts @ [CReturn arg] in
    let var2dt = make_hashtable vars in
    CProgram (var2dt, dt, new_defs, new_stmts)
  | _ -> flatten_error "Flatten: program does not have type"
