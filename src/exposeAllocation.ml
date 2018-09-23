open RProgram
open Gensym
open List

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
    TypeIs (vdt, RLet ("_",
          make_tvoid (RVectorSet (TypeIs (vdt, RVar v), index, TypeIs (xdt, RVar x))),
          gen_vec_sets v vdt (tl xdts) t))

(*
Generates:
(let ([_ (if (< (+ (global-value free_ptr) bytes)
             (global-value fromspace_end))
         (void)
         (collect bytes))])
(let ([v (allocate len type)])
*)
let gen_if_expr vecsets dt vec_name =
  let len = List.length dt in
  let bytes = 8 + (len * 8) in
  let if_expr = make_tvoid (
    RIf (make_tbool (RCmp ("<", make_tint (RBinOp ("+", make_tint (RGlobalValue "freeptr"), make_tint (RInt bytes))), make_tint (RGlobalValue "fromspace_end"))),
    make_tvoid RVoid, make_tvoid (RCollect bytes)))
  in
  let allocate_expr = make_tvec dt (
    RLet (vec_name, make_tvec dt (RAllocate (len, TypeVector dt)), vecsets))
  in
  make_tvec dt (RLet ("_", if_expr, allocate_expr))

(*
Generates:
(let([x0 e0])...(let([xn-1 en-1])
*)
let rec gen_exp_sets xs2es ifexp dt =
  match xs2es with
  | [] -> ifexp
  | ((i, x), e) :: t ->
    make_tvec dt (RLet (x, e, gen_exp_sets t ifexp dt))

let rec expose_exp_type e =
  match e with
  | TypeIs (Some TypeVector dt, RVector es) ->
    let xs = List.mapi (fun index e -> (index, Gensym.gen_str "x")) es in
    let xs2es = List.combine xs es in
    let vec_name = Gensym.gen_str "v" in
    let vector_sets = gen_vec_sets vec_name (Some (TypeVector dt)) dt xs in
    let if_expr = gen_if_expr vector_sets dt vec_name in
    let exp_sets = gen_exp_sets xs2es if_expr dt in
    exp_sets
  | TypeIs (dt, e) -> TypeIs (dt, expose_exp e)

and expose_exp e =
  match e with
  | RVectorRef (v, i) -> RVectorRef (expose_exp_type v, i)
  | RVectorSet (v, i, e) -> RVectorSet (expose_exp_type v, i, expose_exp_type e)
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
  | _ -> e

let expose_allocation program =
  match program with
  | RProgram (dt, e) -> RProgram (dt, expose_exp_type e)
