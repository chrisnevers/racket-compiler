type datatype =
  | TypeInt
  | TypeBool
  | TypeUnit

type rexp =
  | RVar of string
  | RInt of int
  | RBool of bool
  | RAnd of rexp * rexp
  | ROr of rexp * rexp
  | RNot of rexp
  | RIf of rexp * rexp * rexp
  | RCmp of string * rexp * rexp
  | RUnOp of string * rexp
  | RBinOp of string * rexp * rexp
  | RLet of string * rexp * rexp
  | RRead

type rprogram =
  | RProgram of datatype * rexp

let string_of_datatype dt : string =
  match dt with
  | TypeInt -> "int"
  | TypeBool -> "bool"
  | TypeUnit -> "()"

let rec string_of_rexp e : string =
  "(" ^ (fun e ->
  match e with
  | RVar v -> "Var " ^ v
  | RInt i -> "Int " ^ (string_of_int i)
  | RBool b -> "Bool " ^ (string_of_bool b)
  | RAnd (l, r) -> "And " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | ROr (l, r) -> "Or " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | RNot e -> "Not " ^ (string_of_rexp e)
  | RIf (cnd, thn, els) -> "If " ^ (string_of_rexp cnd) ^ " then " ^ (string_of_rexp thn) ^ " else " ^ (string_of_rexp els)
  | RCmp (o, l, r) -> o ^ " " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | RUnOp (o, e) -> o ^ " " ^ (string_of_rexp e)
  | RBinOp (o, l, r) -> o ^ " " ^ (string_of_rexp l) ^ " " ^ (string_of_rexp r)
  | RLet (v, i, b) -> "Let ([Var " ^ v ^ " " ^ (string_of_rexp i) ^ "]) " ^ (string_of_rexp b)
  | RRead -> "Read"
  ) e
  ^ ")"

let print_rprogram p =
  match p with
  | RProgram (dt, e) -> print_endline ("Program : " ^ (string_of_datatype dt) ^ " " ^ (string_of_rexp e))
