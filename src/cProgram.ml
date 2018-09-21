open RProgram

type ccmp =
  | CEq | CL | CLE | CG | CGE

type carg =
  | CInt of int
  | CVar of string
  | CBool of bool
  | CVoid

type cexp =
  | CArg of carg
  | CRead
  | CPrint of datatype * carg
  | CUnOp of string * carg
  | CBinOp of string * carg * carg
  | CNot of carg
  | CCmp of ccmp * carg * carg

type cstmt =
  | CAssign of string * cexp
  | CReturn of carg
  | CIf of cexp * cstmt list * cstmt list

type cprogram =
  | CProgram of string list * datatype * cstmt list

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
  | CVar v -> "Var " ^ v
  | CBool b -> "Bool " ^ (string_of_bool b)
  | CVoid -> "Void"
  ) a
  ^ ")"

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
  ) a
  ^ ")"

let string_of_string_list l : string =
  List.fold_left (fun acc s -> acc ^ s ^ " ") "" l

let print_cprogram program =
  match program with
  | CProgram (vars, dt, stmts) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype dt) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nStmts\t: \n\t[\n\t" ^ (string_of_cstmts stmts) ^ "]"
    )
