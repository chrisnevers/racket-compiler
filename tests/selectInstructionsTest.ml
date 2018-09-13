open OUnit
open SelectInstructions
open AProgram

let select_exp_carg_test = (fun () ->
  assert_equal [Movq (AInt 5, AVar "v")] (select_exp (CArg (CInt 5)) (AVar "v"));
  assert_equal [Movq (AVar "c", AVar "v")] (select_exp (CArg (CVar "c")) (AVar "v"));
  assert_equal [Movq (AInt 1, AVar "v")] (select_exp (CArg (CBool true)) (AVar "v"));
)

let select_exp_cread_test = (fun () ->
  assert_equal [Callq "read_int"; Movq (Reg Rax, AVar "v")] (select_exp CRead (AVar "v"));
)

let select_exp_cunop_test = (fun () ->
  assert_equal [Negq (AVar "v")] (select_exp (CUnOp ("-", CVar "v")) (AVar "v"));
  assert_equal [Movq (AVar "b", AVar "v"); Negq (AVar "v")] (select_exp (CUnOp ("-", CVar "b")) (AVar "v"));
)

let select_exp_cbinop_test = (fun () ->
  assert_equal [Movq (AVar "b", AVar "v"); Addq (AVar "c", AVar "v");] (select_exp (CBinOp ("+", CVar "b", CVar "c")) (AVar "v"));
)

let select_exp_cnot_test = (fun () ->
  assert_equal [Movq (AVar "b", AVar "v"); Xorq (AInt 1, AVar "v");] (select_exp (CNot (CVar "b")) (AVar "v"));
)

let select_exp_ccmp_test = (fun () ->
  assert_equal [Cmpq (AVar "b", AInt 1); Set (AE, ByteReg Al); Movzbq (ByteReg Al, AVar "v")] (select_exp (CCmp (CEq, CBool true, CVar "b")) (AVar "v"));
)

let select_stmts_cassign_test = (fun () ->
  assert_equal [Movq (AVar "v", AVar "x")] (select_stmts [CAssign("x", CArg (CVar "v"))])
)

let select_stmts_creturn_test = (fun () ->
  assert_equal [Movq (AVar "v", Reg Rax)] (select_stmts [CReturn (CVar "v")])
)

let select_stmts_cif_test = (fun () ->
  assert_equal
    [AIf ((AE, AInt 3, AInt 4), [Movq (AInt 1, AVar "v")] , [], [Movq (AInt 2,  AVar "v")], [])]
    (select_stmts [CIf (CCmp (CEq, CInt 3, CInt 4), [CAssign ("v", CArg (CInt 1))], [CAssign ("v", CArg (CInt 2))])])
)

let select_stmts_cif_exn_test = (fun () ->
    let select_stmts_fun = fun () -> select_stmts [CIf (CNot (CInt 5), [CAssign ("v", CArg (CInt 1))], [CAssign ("v", CArg (CInt 2))])] in
   assert_raises ~msg:"Throws error if IF stmt does not have cmp in condition"
     (SelectInstructionError "select_stmt: If statement must use compare to true in condition") select_stmts_fun
)

let suite =
  print_endline "Select Instructions";
  "Select Instructions" >:::
  [
    "select_exp CArg" >:: select_exp_carg_test;
    "select_exp CRead" >:: select_exp_cread_test;
    "select_exp CUnOp" >:: select_exp_cunop_test;
    "select_exp CBinOp" >:: select_exp_cbinop_test;
    "select_exp CNot" >:: select_exp_cnot_test;
    "select_exp CCmp" >:: select_exp_ccmp_test;
    "select_stmts CAssign" >:: select_stmts_cassign_test;
    "select_stmts CReturn" >:: select_stmts_creturn_test;
    "select_stmts CIf" >:: select_stmts_cif_test;
    "select_stmts CIf exn" >:: select_stmts_cif_exn_test;
  ]

let select_instructions_tests =
  run_test_tt_main suite
