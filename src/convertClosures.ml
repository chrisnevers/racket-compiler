let convert_closures program = program
  (* match program with
  | RProgram (dt, defs, exp) ->
    let lifted_defs = ref [] in
    let converted_defs = convert_defs defs lifted_defs in
    let converted_exps = convert_typed_exp exp lifted_defs in
    RProgram (dt, !lifted_defs @ converted_defs, converted_exps) *)
