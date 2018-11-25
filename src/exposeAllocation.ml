open RProgram
open Registers
open Gensym
open List

exception ExposeAllocation of string
let expose_error msg = raise (ExposeAllocation msg)

let rec length_of_datatype e =
  match e with
  | TypeIs (Some TypeInt, _) | TypeIs (Some TypeChar, _) | TypeIs (Some TypeBool, _)
  | TypeIs (Some TypeVoid, _) | TypeIs (Some TypeFunction (_, _), _) -> 8
  | TypeIs (Some TypeVector dt, RVector es) -> 8 + (fold_left (fun acc e -> acc + length_of_datatype e) 0 es)
  | TypeIs (Some TypeArray dt, RArray (len, es)) ->
    let size = length_of_datatype (hd es) in
    (* tag + array-length + (num-of-elements * size-of-elements) *)
    8 + 8 + (size * len)
  | _ -> expose_error "expected int, bool, void, fun, vector, or array datatype"

(*
Generates:
(let ([_ (vector-set! v 0 x0)]) ... (let ([_ (vector-set! v n-1 xn-1)])
  v) ... ))))
*)
let rec gen_vec_sets v vdt xdts xs =
  match xs with
  | [] -> TypeIs (vdt, RVar v)
  | (index, x) :: t ->
    let xdt = Some (hd xdts) in
    TypeIs (vdt, RLet (Gensym.gen_str "_",
          make_tvoid (RVectorSet (TypeIs (vdt, RVar v), index, TypeIs (xdt, RVar x))),
          gen_vec_sets v vdt (tl xdts) t))

(*
Generates:
(let ([_ (array-set! v 0 x0)]) ... (let ([_ (array-set! v n-1 xn-1)])
  v) ... ))))
*)
let rec gen_arr_sets v dt xs =
  match xs with
  | [] -> TypeIs (Some (TypeArray dt), RVar v)
  | (index, x) :: t ->
    TypeIs (Some (TypeArray dt), RLet (Gensym.gen_str "_",
          make_tvoid (RArraySet (TypeIs (Some (TypeArray dt), RVar v), TypeIs (Some TypeInt, RInt (index - 1)), TypeIs (Some dt, RVar x))),
          gen_arr_sets v dt t))

(*
Generates:
(let ([_ (if (< (+ (global-value free_ptr) bytes)
             (global-value fromspace_end))
         (void)
         (collect bytes))])
(let ([v (allocate len type)])
*)
let gen_if_expr vecsets dt vec_name =
  let len = length dt in
  let bytes = 8 + (len * 8) in
  let if_expr = make_tvoid (
    RIf (make_tbool (RCmp ("<", make_tint (RBinOp ("+", make_tint (RGlobalValue free_ptr), make_tint (RInt bytes))), make_tint (RGlobalValue fromspace_end))),
    make_tvoid RVoid, make_tvoid (RCollect bytes)))
  in
  let allocate_expr = make_tvec dt (
    RLet (vec_name, make_tvec dt (RAllocate (len, TypeVector dt)), vecsets))
  in
  make_tvec dt (RLet (Gensym.gen_str "_", if_expr, allocate_expr))


let gen_arr_if_expr array_sets dt arr_name e len =
  let bytes = length_of_datatype e in
  let if_expr = make_tvoid (
    RIf (make_tbool (RCmp ("<", make_tint (RBinOp ("+", make_tint (RGlobalValue free_ptr), make_tint (RInt bytes))), make_tint (RGlobalValue fromspace_end))),
    make_tvoid RVoid, make_tvoid (RCollect bytes)))
  in
  let allocate_expr = make_tarr dt (
    RLet (arr_name, make_tarr dt (RAllocate (len, TypeArray dt)), array_sets))
  in
  make_tarr dt (RLet (Gensym.gen_str "_", if_expr, allocate_expr))

(*
  Ensure requested index is valid.
  Also since the array will essentially have an extra element at the front (its length)
  offset all array-sets by (- 1).
 *)
let gen_array_op array index expr =
  let tmp1 = Gensym.gen_str "len" in
  let indx = Gensym.gen_str "index" in
  let indx_var = make_tint (RVar indx) in
  let tmp1_var = make_tint (RVar tmp1) in
  (* Array index must be less than length of array *)
  let bound_check = make_tbool  (RCmp (">=", indx_var, tmp1_var)) in
  (* Array index must be greater or equal than zero *)
  let pos_check = make_tbool  (RCmp ("<", indx_var, make_tint (RInt 0))) in
  (* Check both bound conditions *)
  let cond = make_tbool  (ROr  (bound_check, pos_check)) in
  (* Throws an unrecoverable runtime error *)
  let error_func = make_tfun [TypeInt; TypeInt] TypeVoid (RFunctionRef "array_access_error") in
  let throw_error = make_tvoid  (RApply (error_func, [tmp1_var; indx_var])) in
  let succ = match expr with
  | `Ref -> make_tvoid (RArrayRef (array, indx_var))
  | `Set exp -> make_tvoid (RArraySet (array, indx_var, exp))
  in
  let check_set   = make_tvoid  (RIf  (cond, throw_error, succ)) in
  RLet (tmp1, make_tint (RArrayRef (array, make_tint (RInt (- 1)))), make_tvoid (RLet (indx, index, check_set)))

(*
Generates:
(let([x0 e0])...(let([xn-1 en-1])
*)
let rec gen_exp_sets xs2es ifexp dt =
  match xs2es with
  | [] -> ifexp
  | ((i, x), e) :: t ->
    TypeIs (dt, RLet (x, expose_exp_type e, gen_exp_sets t ifexp dt))

and expose_exp_type e =
  match e with
  | TypeIs (Some TypeVector dt, RVector es) ->
    let xs = mapi (fun index e -> (index, Gensym.gen_str "x")) es in
    let xs2es = combine xs es in
    let vec_name = Gensym.gen_str "v" in
    let vector_sets = gen_vec_sets vec_name (Some (TypeVector dt)) dt xs in
    let if_expr = gen_if_expr vector_sets dt vec_name in
    let exp_sets = gen_exp_sets xs2es if_expr (Some (TypeVector dt)) in
    exp_sets
  | TypeIs (Some TypeArray dt, RArray (len, es)) ->
    (* Prepend array length to elements *)
    let nes = (make_tint (RInt len) :: es) in
    let xs = mapi (fun index e -> (index, Gensym.gen_str "x")) nes in
    let xs2es = combine xs nes in
    let arr_name = Gensym.gen_str "arr" in
    let array_sets = gen_arr_sets arr_name dt xs in
    let if_expr = gen_arr_if_expr array_sets dt arr_name e (len + 1) in
    let exp_sets = gen_exp_sets xs2es if_expr (Some (TypeArray dt)) in
    exp_sets
  | TypeIs (Some (TypePlus (ldt, rdt)), RInl (e, dt)) ->
    let vec_exp = RVector [TypeIs (Some TypeInt, RInt 0); e; TypeIs (Some rdt, RBool false)] in
    let vec_dt = Some (TypeVector [TypeInt; ldt; rdt]) in
    let vec = TypeIs (vec_dt, vec_exp) in
    expose_exp_type vec
  | TypeIs (Some (TypePlus (ldt, rdt)), RInr (dt, e)) ->
    let vec_exp = RVector [TypeIs (Some TypeInt, RInt 1); TypeIs (Some ldt, RBool false); e] in
    let vec_dt = Some (TypeVector [TypeInt; ldt; rdt]) in
    let vec = TypeIs (vec_dt, vec_exp) in
    expose_exp_type vec
  | TypeIs (dt, e) -> TypeIs (dt, expose_exp e)

and expose_exp e =
  match e with
  | RApply (id, args) -> RApply (id, List.map (fun a -> expose_exp_type a) args)
  | RArray (len, a) -> RArray (len, List.map (fun ve -> expose_exp_type ve) a)
  (* These array-sets are from the programmer. So the array is wrapped in a tuple *)
  | RArraySet (a, i, e) -> gen_array_op (expose_exp_type a) (expose_exp_type i) (`Set (expose_exp_type e))
  | RArrayRef (a, i) -> gen_array_op (expose_exp_type a) (expose_exp_type i) `Ref
  | RVector v -> RVector (List.map (fun ve -> expose_exp_type ve) v)
  | RVectorRef (v, i) -> RVectorRef (expose_exp_type v, i)
  | RVectorSet (v, i, e) -> RVectorSet (expose_exp_type v, i, expose_exp_type e)
  | RVectorLength v -> RVectorLength (expose_exp_type v)
  | RAnd (l, r) -> RAnd (expose_exp_type l, expose_exp_type r)
  | ROr (l, r) -> ROr (expose_exp_type l, expose_exp_type r)
  | RNot e -> RNot (expose_exp_type e)
  | RIf (c, t, f) -> RIf (expose_exp_type c, expose_exp_type t, expose_exp_type f)
  | RCmp (o, l, r) -> RCmp (o, expose_exp_type l, expose_exp_type r)
  | RUnOp (o, e) -> RUnOp (o, expose_exp_type e)
  | RBinOp (o, l, r) -> RBinOp (o, expose_exp_type l, expose_exp_type r)
  | RLet (v, i, b) -> RLet (v, expose_exp_type i, expose_exp_type b)
  | RPrint e -> RPrint (expose_exp_type e)
  | RWhile (c, e) -> RWhile (expose_exp_type c, expose_exp_type e)
  | RCase (e, cases) ->
    let e_vec_id = Gensym.gen_str "case_e" in
    let e_ty_id = Gensym.gen_str "case_ty" in
    let gen_cases = expose_cases cases e_vec_id e_ty_id in
    let vec_ref = TypeIs (Some TypeInt, RVectorRef (e, 0)) in
    let get_ty = TypeIs (Some TypeVoid, RLet (e_ty_id, vec_ref, gen_cases)) in
    let new_cases = RLet (e_vec_id, e, get_ty) in
    expose_exp new_cases
  | _ -> e

and expose_cases cases vec_id ty_id =
  match cases with
  | [] -> TypeIs (Some TypeVoid, RApply (TypeIs (Some TypeVoid, RFunctionRef "match_error"), []))
  | (TypeIs (Some (TypePlus (ldt, rdt)),
      RInl (TypeIs (_, RVar id), _)),
      TypeIs (ret_ty, e)) :: t ->
    let ty_var = TypeIs (Some TypeInt, RVar ty_id) in
    let zero = TypeIs (Some TypeInt, RInt 0) in
    let cmp_types = TypeIs (Some TypeBool, RCmp ("eq?", ty_var, zero)) in
    let vec_ty = Some (TypeVector [TypeInt; ldt; rdt]) in
    let vec_ref = TypeIs (Some ldt, RVectorRef (TypeIs (vec_ty, RVar vec_id), 1)) in
    let assign_and_exec = TypeIs (ret_ty, RLet (id, vec_ref, TypeIs (ret_ty, e))) in
    let els = expose_cases t vec_id ty_id in
    TypeIs (ret_ty, RIf (cmp_types, assign_and_exec, els))
  | (TypeIs (Some (TypePlus (ldt, rdt)),
      RInr (_, TypeIs (_, RVar id))),
      TypeIs (ret_ty, e)) :: t ->
    let ty_var = TypeIs (Some TypeInt, RVar ty_id) in
    let zero = TypeIs (Some TypeInt, RInt 1) in
    let cmp_types = TypeIs (Some TypeBool, RCmp ("eq?", ty_var, zero)) in
    let vec_ty = Some (TypeVector [TypeInt; ldt; rdt]) in
    let vec_ref = TypeIs (Some rdt, RVectorRef (TypeIs (vec_ty, RVar vec_id), 2)) in
    let assign_and_exec = TypeIs (ret_ty, RLet (id, vec_ref, TypeIs (ret_ty, e))) in
    let els = expose_cases t vec_id ty_id in
    TypeIs (ret_ty, RIf (cmp_types, assign_and_exec, els))

let rec expose_defs defs =
  match defs with
  | RDefine (id, args, ret_type, body) :: t ->
    RDefine (id, args, ret_type, expose_exp_type body) :: expose_defs t
  | RDefType (id, dt) :: t -> RDefType (id, dt) :: expose_defs t
  | RTypeCons (id, side, dt) :: t -> RTypeCons (id, side, dt) :: expose_defs t
  | [] -> []

let expose_allocation program =
  match program with
  | RProgram (dt, defs, e) -> RProgram (dt, expose_defs defs, expose_exp_type e)
