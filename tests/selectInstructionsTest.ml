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
    (*"select_stmts CAssign" >:: select_stmts_cassign_test;
    "select_stmts CReturn" >:: select_stmts_creturn_test;
    "select_stmts CIf" >:: select_stmts_cif_test; *)
  ]

let select_instructions_tests =
  run_test_tt_main suite
