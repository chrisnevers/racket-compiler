open AProgram

exception SelectInstructionError of string
let select_instruction_error s = raise (SelectInstructionError s)

let select_stmts stmt =
  match stmt with
  | CAssign (v, e) :: t ->
  | CReturn a :: t -> 
  | CIf (CCmp(o, l, r), thn, els) :: t ->
  | CIf (_, thn, els) :: t -> select_instruction_error "select_stmt: If statement must use compare in condition"
  | [] -> []

let select_instructions program : pprogram =
  match program with
  | CProgram (vars, datatype, stmts) ->
    PProgram (vars, datatype, select_stmts stmts) 
