type datatype =
  | TypeInt
  | TypeChar
  | TypeBool
  | TypeVoid
  | TypeArray of datatype
  | TypeVector of datatype list
  | TypeFunction of datatype list * datatype

type rexp_type =
  | TypeIs of datatype option * rexp

and rexp =
  | RVar of string
  | RInt of int
  | RChar of char
  | RBool of bool
  | RVoid
  | RFunctionRef of string
  | RArray of int * rexp_type list
  | RArraySet of rexp_type * rexp_type * rexp_type
  | RArrayRef of rexp_type * rexp_type
  | RVector of rexp_type list
  | RVectorRef of rexp_type * int
  | RVectorSet of rexp_type * int * rexp_type
  | RVectorLength of rexp_type
  | RCollect of int
  | RAllocate of int * datatype
  | RGlobalValue of string
  | RRead
  | RAnd of rexp_type * rexp_type
  | ROr of rexp_type * rexp_type
  | RNot of rexp_type
  | RIf of rexp_type * rexp_type * rexp_type
  | RCmp of string * rexp_type * rexp_type
  | RUnOp of string * rexp_type
  | RBinOp of string * rexp_type * rexp_type
  | RLet of string * rexp_type * rexp_type
  | RBegin of rexp_type list
  | RWhen of rexp_type * rexp_type list
  | RUnless of rexp_type * rexp_type list
  | RPrint of rexp_type
  | RWhile of rexp_type * rexp_type
  | RApply of rexp_type * rexp_type list
  | RLambda of (string * datatype) list * datatype * rexp_type

type rdefine =
  | RDefine of string * (string * datatype) list * datatype * rexp_type

type rprogram =
  | RProgram of datatype option * rdefine list * rexp_type

let get_datatype_option et : datatype option =
  match et with
  | TypeIs (dt, _) -> dt

let rec get_datatype_options l : datatype option list =
  match l with
  | TypeIs (dt, _) :: tl -> dt :: get_datatype_options tl
  | [] -> []

exception DatatypeError of string
let datatype_error msg = raise (DatatypeError msg)

let get_datatype et : datatype =
  match et with
  | TypeIs (Some dt, _) -> dt
  | _ -> datatype_error "datatype is none"

let rec get_datatypes l : datatype list =
  match l with
  | TypeIs (Some dt, _) :: tl -> dt :: get_datatypes tl
  | [] -> []
  | _ -> datatype_error "datatype is none"

let make_tint e = TypeIs (Some TypeInt, e)
let make_tchar e = TypeIs (Some TypeChar, e)
let make_tbool e = TypeIs (Some TypeBool, e)
let make_tvoid e = TypeIs (Some TypeVoid, e)
let make_tvec dt e = TypeIs (Some (TypeVector dt), e)
let make_tarr dt e = TypeIs (Some (TypeArray dt), e)
let make_tfun args ret e = TypeIs (Some (TypeFunction (args, ret)), e)
let make_tnone e = TypeIs (None, e)

let rec string_of_datatype dt =
  match dt with
  | TypeInt -> "int"
  | TypeChar -> "char"
  | TypeBool -> "bool"
  | TypeVoid -> "void"
  | TypeVector datatypes -> "(" ^ string_of_datatypes datatypes ^ ")"
  | TypeArray datatype -> "[" ^ string_of_datatype datatype ^ "]"
  | TypeFunction (args, ret) -> (List.fold_left (fun acc e -> string_of_datatype e ^ " -> ") "" args) ^ string_of_datatype ret

and string_of_datatypes dt =
  match dt with
  | h :: [] -> string_of_datatype h
  | h :: t -> string_of_datatype h ^ " * " ^ string_of_datatypes t
  | [] -> ""

let rec string_of_datatype_option dt : string =
  match dt with
  | Some a -> string_of_datatype a
  | None -> "None"

and string_of_datatype_options (dt: datatype option list) =
  match dt with
  | h :: [] -> string_of_datatype_option h
  | h :: t -> string_of_datatype_option h ^ " * " ^ string_of_datatype_options t
  | [] -> ""

let rec string_of_rvector_values e =
  match e with
  | TypeIs (_, h) :: [] -> string_of_rexp_value h
  | TypeIs (_, h) :: t -> string_of_rexp_value h ^ ", " ^ string_of_rvector_values t
  | [] -> ""

and string_of_rexp_value e : string =
  match e with
  | RInt i -> string_of_int i
  | RChar c -> Char.escaped c
  | RBool b -> if b then "#t" else "#f"
  | RVoid -> "void"
  | RVector ve -> "(" ^ string_of_rvector_values ve ^ ")"
  | RArray (len, ve) -> "(array (len " ^ string_of_int len ^ ") " ^ string_of_rvector_values ve ^ ")"
  | _ -> ""

let rec string_of_rexp e : string =
  "(" ^
  (fun e ->
  match e with
  | RVar v -> "Var " ^ v
  | RInt i -> "Int " ^ string_of_int i
  | RChar c -> "Char " ^ Char.escaped c
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
  | RVector e -> "(vector " ^ string_of_rexps_type e ^ ")"
  | RArray (len, e) -> "(array (len " ^ string_of_int len ^ ") " ^ string_of_rexps_type e ^ ")"
  | RArraySet (e, i, n) -> "array-set! (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_rexp_type i) ^ ", " ^ (string_of_rexp_type n) ^ ")"
  | RArrayRef (e, i) -> "array-ref (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_rexp_type i) ^ ")"
  | RVectorRef (e, i) -> "Vector-ref (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_int i) ^ ")"
  | RVectorSet (e, i, n) -> "Vector-set! (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_int i) ^ ", " ^ (string_of_rexp_type n) ^ ")"
  | RVectorLength e -> "Vector-length " ^ (string_of_rexp_type e)
  | RCollect i -> "Collect (" ^ string_of_int i ^ ")"
  | RAllocate (i, dt) -> "Allocate (" ^ string_of_int i ^ ", " ^ string_of_datatype dt ^ ")"
  | RGlobalValue s -> "Global-Value (" ^ s ^ ")"
  | RBegin es -> "Begin (" ^ string_of_rexps_type es ^ ")"
  | RWhen (cnd, es) -> "When (" ^ string_of_rexp_type cnd ^ ") (" ^ string_of_rexps_type es ^ ")"
  | RUnless (cnd, es) -> "Unless (" ^ string_of_rexp_type cnd ^ ") (" ^ string_of_rexps_type es ^ ")"
  | RPrint e -> "Print (" ^ string_of_rexp_type e ^ ")"
  | RWhile (cnd, e) -> "While (" ^ string_of_rexp_type cnd ^ ") (" ^ string_of_rexp_type e ^ ")"
  | RApply (id, args) -> string_of_rexp_type id ^ "(" ^ (string_of_rexps_type args) ^ ")"
  | RFunctionRef id -> "FunctionRef" ^ id
  | RLambda _ -> "lambda"
  ) e
  ^ ")\n"

and string_of_rexps exps =
  match exps with
  | h :: [] -> string_of_rexp h
  | h :: t -> string_of_rexp h ^ ", " ^ string_of_rexps t
  | [] -> ""

and string_of_rexp_type e : string =
  match e with
  | TypeIs (dt, e) -> string_of_rexp e
  (* ^ ": " ^ (string_of_datatype_option dt) *)

and string_of_rexps_type e : string =
  match e with
  | h :: [] -> string_of_rexp_type h
  | h :: t -> string_of_rexp_type h ^ ", " ^ string_of_rexps_type t
  | [] -> ""

let print_rprogram p =
  match p with
  | RProgram (dt, defs, e) -> print_endline ("Program :\nDatatype: " ^ (string_of_datatype_option dt) ^ "\nExp:" ^ (string_of_rexp_type e))
