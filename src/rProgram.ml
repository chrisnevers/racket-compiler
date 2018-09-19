type datatype =
  | TypeInt
  | TypeBool
  | TypeVoid
  | TypeVector of datatype list

type rexp_type =
  | TypeIs of datatype * rexp

and rexp =
  | RVar of string
  | RInt of int
  | RBool of bool
  | RVoid
  | RVector of rexp_type list
  | RVectorRef of rexp_type * int
  | RVectorSet of rexp_type * int * rexp_type
  | RRead
  | RAnd of rexp_type * rexp_type
  | ROr of rexp_type * rexp_type
  | RNot of rexp_type
  | RIf of rexp_type * rexp_type * rexp_type
  | RCmp of string * rexp_type * rexp_type
  | RUnOp of string * rexp_type
  | RBinOp of string * rexp_type * rexp_type
  | RLet of string * rexp_type * rexp_type

type rprogram =
  | RProgram of datatype * rexp_type

let get_datatype et : datatype =
  match et with
  | TypeIs (dt, _) -> dt

let rec get_datatypes l : datatype list =
  match l with
  | TypeIs (dt, _) :: tl -> dt :: get_datatypes tl
  | [] -> []

let make_tint e = TypeIs (TypeInt, e)
let make_tbool e = TypeIs (TypeBool, e)
let make_tvoid e = TypeIs (TypeVoid, e)

let rec string_of_datatype dt : string =
  match dt with
  | TypeInt -> "int"
  | TypeBool -> "bool"
  | TypeVoid -> "void"
  | TypeVector datatypes -> "(" ^ string_of_datatypes datatypes ^ ")"

and string_of_datatypes dt =
  match dt with
  | h :: [] -> string_of_datatype h
  | h :: t -> string_of_datatype h ^ " * " ^ string_of_datatypes t
  | [] -> ""

let rec string_of_rexp e : string =
  (* "(" ^ *)
  (fun e ->
  match e with
  | RVar v -> "Var " ^ v
  | RInt i -> string_of_int i
  | RBool b -> if b then "#t" else "#f"
  | RVoid -> "void"
  | RAnd (l, r) -> "And " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | ROr (l, r) -> "Or " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | RNot e -> "Not " ^ (string_of_rexp_type e)
  | RIf (cnd, thn, els) -> "If " ^ (string_of_rexp_type cnd) ^ " then " ^ (string_of_rexp_type thn) ^ " else " ^ (string_of_rexp_type els)
  | RCmp (o, l, r) -> o ^ " " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | RUnOp (o, e) -> o ^ " " ^ (string_of_rexp_type e)
  | RBinOp (o, l, r) -> o ^ " " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | RLet (v, i, b) -> "Let ([Var " ^ v ^ " " ^ (string_of_rexp_type i) ^ "]) " ^ (string_of_rexp_type b)
  | RRead -> "Read"
  | RVector e -> "(" ^ string_of_rexps_type e ^ ")"
  | RVectorRef (e, i) -> "Vector-ref (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_int i) ^ ")"
  | RVectorSet (e, i, n) -> "Vector-set! (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_int i) ^ ", " ^ (string_of_rexp_type n) ^ ")"
  ) e
  (* ^ ")" *)

and string_of_rexps exps =
  match exps with
  | h :: [] -> string_of_rexp h
  | h :: t -> string_of_rexp h ^ ", " ^ string_of_rexps t
  | [] -> ""

and string_of_rexp_type e : string =
  match e with
  | TypeIs (dt, e) -> string_of_rexp e ^ ": " ^ (string_of_datatype dt)

and string_of_rexps_type e : string =
  match e with
  | h :: [] -> string_of_rexp_type h
  | h :: t -> string_of_rexp_type h ^ ", " ^ string_of_rexps_type t
  | [] -> ""

let print_rprogram p =
  match p with
  | RProgram (dt, e) -> print_endline ("Program : " ^ (string_of_datatype dt) ^ " " ^ (string_of_rexp_type e))
