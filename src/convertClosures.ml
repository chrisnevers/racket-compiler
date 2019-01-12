open RProgram
open Gensym
open List
open Helper

exception ClosureError of string
let closure_error msg = raise (ClosureError msg)

let add_args = Hashtbl.create 10

let rec gen_define_lets free_vars exp ret_type vec_type cur =
  match free_vars with
  | (id, dt) :: t -> TypeIs (Some ret_type,
    RLet (id,
    TypeIs (Some dt, RVectorRef (TypeIs (Some vec_type, RVar "fclo"), cur)),
    gen_define_lets t exp ret_type vec_type (cur + 1)))
  | [] -> exp

(* RDefine of string * (string * datatype) list * datatype * rexp_type *)
let gen_define name vec_type vars return exp free_vars vec =
  RDefine (
    name,
    ("fclo", vec_type) :: vars,
    return,
    gen_define_lets free_vars exp return vec_type 1
  )

let get_function_types f =
  match f with
  | TypeFunction (args, ret) -> args, ret
  | _ -> closure_error "expected function type"

let get_vector_types v =
  match v with
  | TypeVector dts -> dts
  | _ -> closure_error ("expected vector type : " ^ string_of_datatype v)

let rec convert_typed_exp exp defs =
  match exp with
  | TypeIs (Some dt, RFunctionRef l) ->
    Hashtbl.replace add_args l (TypeVector [dt]);
    TypeIs (Some (TypeVector [dt]), RVector [exp])
  | TypeIs (Some (TypeFunction (arg_dts, ret_dt)), RVar v) ->
    let vec_type = TypeVector [TypeFunction (arg_dts, ret_dt)] in
    Hashtbl.replace add_args v vec_type;
    TypeIs (Some vec_type, RVar v)
  | TypeIs (dt, RLambda (args, ret, e)) ->
    (* Args has datatype, need to preserve *)
    let ne = convert_typed_exp e defs in
    let free_vars = get_free_vars args ne in
    let free_var_ids, free_var_dts = List.split free_vars in
    let arg_var_ids, arg_var_dts = List.split args in
    let def_name = Gensym.gen_str "clos" in
    let func_type = TypeFunction (arg_var_dts, ret) in
    let code_ptr = TypeIs (Some func_type, RFunctionRef def_name) in
    (* vector (codeptr {free_vars} ) *)
    let es = code_ptr :: (map (fun (v, dt) -> TypeIs (Some dt, RVar v)) free_vars) in
    (* Make vector of datatypes *)
    let vector_arg = TypeVector (func_type :: free_var_dts) in
    let vector = TypeIs (Some vector_arg, RVector es) in
    (* add arg of vector type *)
    let def = gen_define def_name vector_arg args ret ne free_vars vector in
    defs := def :: !defs;
    vector
  | TypeIs (dt, e) -> TypeIs (dt, convert_exp e defs)

and convert_exp exp defs =
  match exp with
  | RVector es -> RVector (map (fun e -> convert_typed_exp e defs) es)
  | RVectorRef (e, i) -> RVectorRef (convert_typed_exp e defs, i)
  | RVectorSet (v, i, e) -> RVectorSet (convert_typed_exp v defs, i, convert_typed_exp e defs)
  | RVectorLength e -> RVectorLength (convert_typed_exp e defs)
  | RAnd (l, r) -> RAnd (convert_typed_exp l defs, convert_typed_exp r defs)
  | ROr (l, r) -> ROr (convert_typed_exp l defs, convert_typed_exp r defs)
  | RNot e -> RNot (convert_typed_exp e defs)
  | RIf (c, t, e) -> RIf (convert_typed_exp c defs, convert_typed_exp t defs, convert_typed_exp e defs)
  | RCmp (o, l, r) -> RCmp (o, convert_typed_exp l defs, convert_typed_exp r defs)
  | RUnOp (o, e) -> RUnOp (o, convert_typed_exp e defs)
  | RBinOp (o, l, r) -> RBinOp (o, convert_typed_exp l defs, convert_typed_exp r defs)
  | RLet (v, i, b) -> RLet (v, convert_typed_exp i defs, convert_typed_exp b defs)
  | RBegin es -> RBegin (map (fun e -> convert_typed_exp e defs) es)
  | RWhen (c, es) -> RWhen (c, map (fun e -> convert_typed_exp e defs) es)
  | RUnless (c, es) -> RUnless (c, map (fun e -> convert_typed_exp e defs) es)
  | RPrint e -> RPrint (convert_typed_exp e defs)
  | RWhile (c, e) -> RWhile (c, convert_typed_exp e defs)
  | RInl (e, dt) -> RInl (convert_typed_exp e defs, dt)
  | RInr (dt, e) -> RInr (dt, convert_typed_exp e defs)
  | RCase (e, cnds) -> RCase (convert_typed_exp e defs, map (fun (a, b) -> (convert_typed_exp a defs, convert_typed_exp b defs)) cnds)
  | RApply (e, es) ->
    let ne = convert_typed_exp e defs in
    let vdt = get_datatype ne in
    let fdt = hd (get_vector_types vdt) in
    let fret, ret = get_function_types fdt in
    let nes = map (fun e -> convert_typed_exp e defs) es in
    let name = Gensym.gen_str "fclo" in
    let apply = TypeIs (Some ret, RApply (TypeIs (Some fdt, RVectorRef (TypeIs (Some vdt, RVar name), 0)), TypeIs (Some vdt, RVar name) :: nes)) in
    RLet (name, ne, apply)
  | RVar _ -> exp
  | RInt _ -> exp
  | RBool _ -> exp
  | RVoid -> exp
  | RCollect _ -> exp
  | RAllocate _ -> exp
  | RGlobalValue _ -> exp
  | RFunctionRef _ -> exp
  | RRead -> exp
  | _ -> exp

let rec convert_defs defs ndefs =
  match defs with
  | RDefine (id, args, ret, body) :: t ->
    let nargs = map (fun (id, dt) ->
    match dt with
    | TypeFunction _ -> (id, TypeVector [dt])
    | _ -> (id, dt)
    ) args in
    RDefine (id, nargs, ret, convert_typed_exp body ndefs) :: convert_defs t ndefs
  | RDefType (id, l, r, dt) :: t -> RDefType (id, l, r, dt) :: convert_defs t ndefs
  | [] -> []

let rec add_vec_args defs =
  match defs with
  | RDefine (id, args, ret, body) :: t ->
    let def = (try
      let dt = Hashtbl.find add_args id in
      RDefine (id, ("fclo", dt) :: args, ret, body)
    with Not_found -> RDefine (id, args, ret, body))
    in
    def :: add_vec_args t
  | RDefType (id, l, r, dt) :: t -> RDefType (id, l, r, dt) :: add_vec_args t
  | [] -> []

let convert_closures program =
  match program with
  | RProgram (dt, defs, exp) ->
    let lifted_defs = ref [] in
    let converted_defs = convert_defs defs lifted_defs in
    let converted_exps = convert_typed_exp exp lifted_defs in
    let new_defs = add_vec_args (!lifted_defs @ converted_defs) in
    let ndt = match dt with
    | Some TypeFunction _ -> Some (TypeVector [get_some dt])
    | _ -> dt
    in
    RProgram (ndt, new_defs, converted_exps)
