open CProgram
open RProgram
open Helper

exception FlattenError of string

let flatten_error s = raise (FlattenError s)

let get_var_name v tmp_count =
  match v with
  | Some name -> name
  | None ->
    tmp_count := !tmp_count + 1;
    "tmp" ^ (string_of_int !tmp_count)

let get_carg_of_rarg a : carg =
  match a with
  | RBool b -> CBool b
  | RInt i -> CInt i
  | RVar name -> CVar name
  | RVoid -> CVoid
  | _ -> flatten_error ("get_carg_of_rarg: Expected to receive CArg but received " ^ (string_of_rexp a))

let get_ccmp_of_rcmp o : ccmp =
  match o with
  | "eq?" -> CEq
  | "<" -> CL
  | "<=" -> CLE
  | ">" -> CG
  | ">=" -> CGE
  | _ -> flatten_error ("get_ccmp_of_rcmp: Expected compare operator but received " ^ o)

let flatten_arg ?(v=None) a tmp_count : carg * cstmt list * string list =
  let flat_arg = get_carg_of_rarg a in
  let stmts = if v = None then [] else [CAssign (get_var_name v tmp_count, CArg flat_arg)] in
  let var_list = [] in
  (flat_arg, stmts, var_list)

let rec flatten_exp ?(v=None) e tmp_count : carg * cstmt list * string list =
  match e with
  | RVar _ | RInt _ | RBool _ | RVoid ->
    flatten_arg e tmp_count ~v:v
  | RAnd (l, r) ->
    let (larg, lstmts, lvars) = flatten_typed_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_typed_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let lif_cnd = CCmp (CEq, CBool true, larg) in
    let rif_cnd = CCmp (CEq, CBool true, rarg) in
    (* We only execute this if first condition is true, so if this condition is true, then set var to true, otherwise false *)
    let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
    (* Execute first condition, if true continue to next condition (execute its stmts then call if to see if true), otherwise short circuit to false *)
    let lif = CIf (lif_cnd, rstmts @ [rif], [CAssign (var_name, CArg (CBool false))]) in
    (* Execute lstmts see if left is true *)
    let stmts = lstmts @ [lif] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)
  | ROr (l, r) ->
    let (larg, lstmts, lvars) = flatten_typed_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_typed_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let lif_cnd = CCmp (CEq, CBool true, larg) in
    let rif_cnd = CCmp (CEq, CBool true, rarg) in
    (* We only execute this if first condition is false, so if this condition is true, then set var to true, otherwise false *)
    let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
    (* Execute first condition, if true then set var to true, else see if next condition is true *)
    let lif = CIf (lif_cnd, [CAssign (var_name, CArg (CBool true))], rstmts @ [rif]) in
    (* Execute lstmts see if left is true *)
    let stmts = lstmts @ [lif] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)
  | RNot e ->
    let (earg, estmts, evars) = flatten_typed_exp e tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = estmts @ [CAssign (var_name, CNot earg)] in
    let var_list = if v = None then var_name :: evars else evars in
    (flat_arg, stmts, var_list)
  | RIf (cnd, thn, els) ->
    let var_name = get_var_name v tmp_count in
    let (cnd_arg, cnd_stmts, cnd_vars) = flatten_typed_exp cnd tmp_count in
    (* Assign result of then and else conditions to lhs variable / or newly created tmp *)
    let (thn_arg, thn_stmts, thn_vars) = flatten_typed_exp thn tmp_count ~v:(Some var_name) in
    let (els_arg, els_stmts, els_vars) = flatten_typed_exp els tmp_count ~v:(Some var_name) in
    let if_cnd = CCmp (CEq, CBool true, cnd_arg) in
    let flat_arg = CVar var_name in
    let stmts = cnd_stmts @ [CIf (if_cnd, thn_stmts, els_stmts)] in
    let var_list = if v = None then var_name :: cnd_vars @ thn_vars @ els_vars else cnd_vars @ thn_vars @ els_vars in
    (flat_arg, stmts, var_list)
  | RWhile (cnd, thn) ->
    let var_name = get_var_name v tmp_count in
    let (cnd_arg, cnd_stmts, cnd_vars) = flatten_typed_exp cnd tmp_count in
    let (thn_arg, thn_stmts, thn_vars) = flatten_typed_exp thn tmp_count ~v:(Some var_name) in
    let while_cnd = CCmp (CEq, CBool true, cnd_arg) in
    let flat_arg = CVar var_name in
    let stmts = [CWhile (cnd_stmts, while_cnd, thn_stmts)] in
    let var_list = if v = None then var_name :: cnd_vars @ thn_vars else cnd_vars @ thn_vars in
    (flat_arg, stmts, var_list)
  | RCmp (o, l, r) ->
    let (larg, lstmts, lvars) = flatten_typed_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_typed_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let ccmp = get_ccmp_of_rcmp o in
    let stmts = lstmts @ rstmts @ [CAssign (var_name, CCmp (ccmp, larg, rarg))] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)
  | RUnOp (o, e) ->
    let (earg, estmts, evars) = flatten_typed_exp e tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = estmts @ [CAssign (var_name, CUnOp (o, earg))] in
    let var_list = if v = None then var_name :: evars else evars in
    (flat_arg, stmts, var_list)
  | RBinOp (o, l, r) ->
    let (larg, lstmts, lvars) = flatten_typed_exp l tmp_count in
    let (rarg, rstmts, rvars) = flatten_typed_exp r tmp_count in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = lstmts @ rstmts @ [CAssign (var_name, CBinOp (o, larg, rarg))] in
    let var_list = if v = None then var_name :: lvars @ rvars else lvars @ rvars in
    (flat_arg, stmts, var_list)
  | RLet (name, i, b) ->
    (* Assign result of inner expression to the variable being declared *)
    let (iarg, istmts, ivars) = flatten_typed_exp i tmp_count ~v:(Some name) in
    (* Assign result of body function to whatever variable this expression is a child of *)
    let (barg, bstmts, bvars) = flatten_typed_exp b tmp_count ~v:v in
    let flat_arg = barg in
    let stmts = istmts @ bstmts in
    let var_list = name :: ivars @ bvars in
    (flat_arg, stmts, var_list)
  | RRead ->
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = [CAssign (var_name, CRead)] in
    let var_list = if v = None then [var_name] else [] in
    (flat_arg, stmts, var_list)
  | RPrint e ->
    let (earg, estmts, evars) = flatten_typed_exp e tmp_count in
    let dt = get_datatype e in
    let var_name = get_var_name v tmp_count in
    let flat_arg = CVar var_name in
    let stmts = estmts @ [CAssign (var_name, CPrint (dt, earg))] in
    let var_list = if v = None then var_name :: evars else evars in
    (flat_arg, stmts, var_list)
  | RVector _ -> flatten_error "vector not implemented"
  | RVectorSet (_, _, _) -> flatten_error "vector-set! implemented"
  | RVectorRef (_, _) -> flatten_error "vector-ref not implemented"
  | RBegin _ -> flatten_error "should not have begin in flatten"
  | RWhen (_, _) -> flatten_error "should not have when in flatten"
  | RUnless (_, _) -> flatten_error "should not have unless in flatten"

and flatten_typed_exp ?(v=None) e tmp_count =
  match e with
  | TypeIs (dt, x) -> flatten_exp ~v x tmp_count

let flatten program : cprogram =
  match program with
  | RProgram (Some dt, e) ->
    let tmp_count = ref 0 in
    let (arg, stmts, vars) = flatten_typed_exp e tmp_count in
    let new_stmts = stmts @ [CReturn arg] in
    CProgram (vars, dt, new_stmts)
  | _ -> flatten_error "Flatten: program does not have type"
