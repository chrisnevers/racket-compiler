open OUnit
open Flatten
open RProgram
open CProgram

let get_var_name_test = (fun () ->
  let tmp_count = ref 0 in
  assert_equal "v" (get_var_name (Some "v") tmp_count);
  assert_equal "tmp1" (get_var_name None tmp_count)
)

let flatten_arg_test = (fun () ->
  let tmp_count = ref 0 in
  let a = RVar "v" in
  assert_equal (CVar "v", [], []) (flatten_arg a tmp_count);
  assert_equal (CVar "v", [CAssign ("name", CArg (CVar "v"))], []) (flatten_arg a tmp_count ~v:(Some "name"))
)

let flatten_and_test = (fun () ->
  let e = RAnd (TypeIs (TypeBool, RBool true), TypeIs (TypeBool, RCmp ("eq?", TypeIs (TypeInt, RInt 1), TypeIs (TypeInt, RInt 5)))) in
  let tmp_count = ref 0 in
  let expected_stmts =
    [CIf (CCmp (CEq, CBool true, CBool true),
    [CAssign ("tmp1", CCmp (CEq, CInt 1, CInt 5));
    CIf (CCmp (CEq, CBool true, CVar "tmp1"),
      [CAssign ("tmp2", CArg (CBool true))],
      [CAssign ("tmp2", CArg (CBool false))])],
    [CAssign ("tmp2", CArg (CBool false))])]
  in
  assert_equal (CVar "tmp2", expected_stmts, ["tmp2"; "tmp1"]) (flatten_exp e tmp_count)
)

let flatten_not_test = (fun () ->
  let tmp_count = ref 0 in
  let e = RNot (TypeIs (TypeBool, RBool false)) in
  assert_equal (CVar "name", [CAssign ("name", CNot (CBool false));], []) (flatten_exp e tmp_count ~v:(Some "name"));
  assert_equal (CVar "tmp1", [CAssign ("tmp1", CNot (CBool false));], ["tmp1"]) (flatten_exp e tmp_count)
)

let flatten_if_test = (fun () ->
  let e = RIf (TypeIs (TypeBool, RBool true), TypeIs (TypeInt, RInt 4), TypeIs (TypeInt, RInt 5)) in
  let tmp_count = ref 0 in
  assert_equal (CVar "tmp1", [CIf (CCmp (CEq, CBool true, CBool true), [CAssign ("tmp1", CArg (CInt 4))],
  [CAssign ("tmp1", CArg (CInt 5))])], ["tmp1"]) (flatten_exp e tmp_count)
)

let flatten_cmp_test = (fun () ->
  let e = RCmp ("<", TypeIs (TypeInt, RInt 4), TypeIs (TypeInt, RInt 5)) in
  let tmp_count = ref 0 in
  assert_equal (CVar "tmp1", [CAssign ("tmp1", CCmp (CL, CInt 4, CInt 5))], ["tmp1"]) (flatten_exp e tmp_count)
)

let flatten_unop_test = (fun () ->
  let e = RUnOp ("-", TypeIs (TypeInt, RInt 4)) in
  let tmp_count = ref 0 in
  assert_equal (CVar "tmp1", [CAssign ("tmp1", CUnOp("-", CInt 4))], ["tmp1"]) (flatten_exp e tmp_count)
)

let flatten_binop_test = (fun () ->
  let e = RBinOp ("+", TypeIs (TypeInt, RInt 4), TypeIs (TypeInt, RInt 5)) in
  let tmp_count = ref 0 in
  assert_equal (CVar "tmp1", [CAssign ("tmp1", CBinOp("+", CInt 4, CInt 5))], ["tmp1"]) (flatten_exp e tmp_count)
)

let flatten_let_test = (fun () ->
  let e = RLet ("v", TypeIs (TypeInt, RInt 4), TypeIs (TypeInt, RVar "v")) in
  let tmp_count = ref 0 in
  assert_equal (CVar "v", [CAssign ("v", CArg (CInt 4))], ["v"]) (flatten_exp e tmp_count)
)

let flatten_read_test = (fun () ->
  let e = RRead in
  let tmp_count = ref 0 in
  assert_equal (CVar "tmp1", [CAssign ("tmp1", CRead)], ["tmp1"]) (flatten_exp e tmp_count);
  assert_equal (CVar "v", [CAssign ("v", CRead)], []) (flatten_exp e tmp_count ~v:(Some "v"))
)

let test =
  print_endline "Flatten";
  "Flatten" >:::
  [
    "get_var_name" >:: get_var_name_test;
    "flatten_arg" >:: flatten_arg_test;
    "flatten_exp and" >:: flatten_and_test;
    "flatten_exp not" >:: flatten_not_test;
    "flatten_exp if" >:: flatten_if_test;
    "flatten_exp cmp" >:: flatten_cmp_test;
    "flatten_exp unop" >:: flatten_unop_test;
    "flatten_exp binop" >:: flatten_binop_test;
    "flatten_exp let" >:: flatten_let_test;
    "flatten_exp read" >:: flatten_read_test;
  ]

let flatten_tests =
  run_test_tt_main test
