open RProgram

type ccmp =
  | CEq | CL | CLE | CG | CGE

type carg =
  | CInt of int
  | CChar of char
  | CVar of string
  | CBool of bool
  | CVoid (* can be 1, so evaluates to true *)
  | CGlobalValue of string
  | CFunctionRef of string

type cexp =
  | CArg of carg
  | CRead
  | CPrint of datatype * carg
  | CUnOp of string * carg
  | CBinOp of string * carg * carg
  | CNot of carg
  | CCmp of ccmp * carg * carg
  | CAlloc of int * datatype
  | CVectorRef of carg * int
  | CArrayRef of carg * carg
  | CApply of carg * carg list

type cstmt =
  | CAssign of string * cexp
  | CReturn of carg
  | CIf of cexp * cstmt list * cstmt list
  | CWhile of cstmt list * cexp * cstmt list
  | CCollect of int
  | CVectorSet of carg * int * carg
  | CArraySet of carg * carg * carg

type var_type = ((string, datatype) Hashtbl.t)

type cdefine =
  | CDefine of string * (string * datatype) list * datatype * var_type * cstmt list
  | CDefType of string * string * string * datatype

type cprogram =
  | CProgram of var_type * datatype * cdefine list * cstmt list

let string_of_ccmp o : string =
  match o with
  | CEq -> "eq?"
  | CL -> "<"
  | CLE -> "<="
  | CG -> ">"
  | CGE -> ">="

let string_of_carg a : string =
  "(" ^ (fun e ->
  match a with
  | CInt i -> "Int " ^ (string_of_int i)
  | CChar c -> "Char " ^ (Char.escaped c)
  | CVar v -> "Var " ^ v
  | CBool b -> "Bool " ^ (string_of_bool b)
  | CVoid -> "Void"
  | CGlobalValue s -> "GlobalValue " ^ s
  | CFunctionRef s -> "FunctionRef " ^ s
  ) a
  ^ ")"

let string_of_carg_type a : string =
  match a with
  | CInt _ -> "int"
  | CChar _ -> "char"
  | CVar _ -> "var"
  | CBool _ -> "bool"
  | CVoid -> "void"
  | CGlobalValue _ -> "glbl"
  | CFunctionRef _ -> "fun"

let string_of_cexp e : string =
  "(" ^ (fun e ->
  match e with
  | CArg a -> "Arg " ^ (string_of_carg a)
  | CRead -> "Read"
  | CPrint (dt, a) -> "Print" ^ (string_of_carg a)
  | CUnOp (o, a) -> "UnOp " ^ o ^ " " ^ (string_of_carg a)
  | CBinOp (o, l, r) -> "BinOp " ^ o ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  | CNot a -> "Not " ^ (string_of_carg a)
  | CCmp (cmp, l, r) -> "Cmp " ^ (string_of_ccmp cmp) ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  | CAlloc (i, dt) -> "Allocate " ^ (string_of_int i) ^ " " ^ (string_of_datatype dt)
  | CVectorRef (v, i) -> "VectorRef " ^ string_of_carg v ^ " " ^ string_of_int i
  | CApply (id, args) -> "Apply " ^ (string_of_carg id) ^ " " ^ List.fold_left (fun acc s -> acc ^ string_of_carg s ^ "\n\t") "" args
  ) e
  ^ ")"

let rec string_of_cstmts stmts : string =
  List.fold_left (fun acc s -> acc ^ string_of_cstmt s ^ "\n\t") "" stmts

and string_of_cstmt a : string =
  "(" ^ (fun e ->
  match a with
  | CAssign (v, e) -> "Assign " ^ v ^ " " ^ (string_of_cexp e)
  | CReturn a -> "Return " ^ (string_of_carg a)
  | CIf (cnd, thn, els) -> "If " ^ (string_of_cexp cnd) ^ "\n\t\t" ^ (string_of_cstmts thn) ^ "\t" ^ (string_of_cstmts els)
  | CWhile (cnds, cnda, thn) -> "While " ^ string_of_cstmts cnds ^ "\n\t\t" ^ string_of_cexp cnda ^ "\n\t\t" ^ string_of_cstmts thn
  | CCollect i -> "Collect " ^ string_of_int i
  | CVectorSet (v, i, e) -> "VectorSet " ^ string_of_carg v ^ " " ^ string_of_int i ^ " " ^ string_of_carg e
  | CArraySet (v, i, e) -> "ArraySet " ^ string_of_carg v ^ " " ^ string_of_carg i ^ " " ^ string_of_carg e
  ) a
  ^ ")"

let string_of_string_list l : string =
    List.fold_left (fun acc s -> acc ^ s ^ " ") "" l

let string_of_vars_list l : string =
  List.fold_left (fun acc s -> match s with
  | (name, dt) -> acc ^ "(" ^ name ^ ": " ^ string_of_datatype dt ^ ")") "" l

let print_cprogram program =
  match program with
  | CProgram (vars, dt, defs, stmts) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype dt) ^
      (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nStmts\t: \n\t[\n\t" ^ (string_of_cstmts stmts) ^ "]"
    )
