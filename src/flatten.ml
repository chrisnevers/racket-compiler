open CProgram
open RProgram

let flatten_exp e : carg * cstmt list * string list =
  (CInt 0, [CReturn (CInt 0)], [])

let flatten program : cprogram =
  match program with
  | RProgram (dt, e) ->
    let (arg, stmts, vars) = flatten_exp e in
    CProgram (vars, dt, stmts)
