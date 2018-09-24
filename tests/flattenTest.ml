open OUnit
open Flatten
open RProgram
open CProgram
open Gensym

let print_res e =
  match e with
  | (arg, stmts, vars) ->
    print_endline ("arg: " ^ string_of_carg arg);
    print_string "stmts: ";
    List.iter (fun e -> print_endline (string_of_cstmt e)) stmts;
    print_string ("vars: ");
    List.iter (fun e -> match e with (name, dt) -> print_endline (name ^ " : " ^ (string_of_datatype dt))) vars

let get_var_name_test = (fun () ->
  Gensym.reset();
  assert_equal "v" (get_var_name (Some "v") "tmp");
  assert_equal "tmp0" (get_var_name None "tmp");
)

let flatten_arg_test = (fun () ->
  Gensym.reset();
  let a = RVar "v" in
  assert_equal (CVar "v", [], []) (flatten_arg a);
  assert_equal (CVar "v", [CAssign ("name", CArg (CVar "v"))], []) (flatten_arg a ~v:(Some "name"))
)

let flatten_and_test = (fun () ->
  Gensym.reset();
  let e = make_tbool (RAnd (make_tbool (RBool true), make_tbool (RCmp ("eq?", make_tint (RInt 1), make_tint (RInt 5))))) in
  let actual = flatten_typed_exp e in
  let ev1 = "and1" in
  let ev2 = "cmp0" in
  let expected_stmts =
    [CIf (CCmp (CEq, CBool true, CBool true),
    [CAssign (ev2, CCmp (CEq, CInt 1, CInt 5));
    CIf (CCmp (CEq, CBool true, CVar ev2),
      [CAssign (ev1, CArg (CBool true))],
      [CAssign (ev1, CArg (CBool false))])],
    [CAssign (ev1, CArg (CBool false))])]
  in
  assert_equal (CVar ev1, expected_stmts, [(ev1, TypeBool); (ev2, TypeBool)]) actual
)

let flatten_not_test = (fun () ->
  Gensym.reset();
  let e = make_tbool (RNot (make_tbool (RBool false))) in
  let actual = flatten_typed_exp e ~v:(Some "name") in
  assert_equal (CVar "name", [CAssign ("name", CNot (CBool false));], []) actual;
  let actual = flatten_typed_exp e in
  let ev = "not0" in
  assert_equal (CVar ev, [CAssign (ev, CNot (CBool false));], [(ev, TypeBool)]) actual
)

let flatten_if_test = (fun () ->
  Gensym.reset();
  let e = make_tint (RIf (make_tbool (RBool true), make_tint (RInt 4), make_tint (RInt 5))) in
  let actual = flatten_typed_exp e in
  let ev = "if0" in
  assert_equal (CVar ev, [CIf (CCmp (CEq, CBool true, CBool true), [CAssign (ev, CArg (CInt 4))],
  [CAssign (ev, CArg (CInt 5))])], [(ev, TypeInt)]) actual
)

let flatten_cmp_test = (fun () ->
  Gensym.reset();
  let e = make_tbool (RCmp ("<", make_tint (RInt 4), make_tint (RInt 5))) in
  let actual = flatten_typed_exp e in
  let ev = "cmp0" in
  assert_equal (CVar ev, [CAssign (ev, CCmp (CL, CInt 4, CInt 5))], [(ev, TypeBool)]) actual
)

let flatten_unop_test = (fun () ->
  Gensym.reset();
  let e = make_tint (RUnOp ("-", make_tint (RInt 4))) in
  let actual = flatten_typed_exp e in
  let ev = "unop0" in
  assert_equal (CVar ev, [CAssign (ev, CUnOp("-", CInt 4))], [(ev, TypeInt)]) actual
)

let flatten_binop_test = (fun () ->
  Gensym.reset();
  let e = make_tint (RBinOp ("+", make_tint (RInt 4), make_tint (RInt 5))) in
  let actual = flatten_typed_exp e in
  let ev = "binop0" in
  assert_equal (CVar ev, [CAssign (ev, CBinOp("+", CInt 4, CInt 5))], [(ev, TypeInt)]) actual
)

let flatten_let_test = (fun () ->
  Gensym.reset();
  let e = make_tint (RLet ("v", make_tint (RInt 4), make_tint (RVar "v"))) in
  let actual = flatten_typed_exp e in
  assert_equal (CVar "v", [CAssign ("v", CArg (CInt 4))], [("v", TypeInt)]) actual
)

let flatten_read_test = (fun () ->
  Gensym.reset();
  let e = make_tint (RRead) in
  let actual = flatten_typed_exp e in
  let ev = "read0" in
  assert_equal (CVar ev, [CAssign (ev, CRead)], [(ev, TypeInt)]) actual;
  assert_equal (CVar "v", [CAssign ("v", CRead)], []) (flatten_typed_exp e ~v:(Some "v"))
)

let test =
  print_endline "Flatten";
  "Flatten" >:::
  [
    "get_var_name" >:: get_var_name_test;
    "flatten_arg" >:: flatten_arg_test;
    "flatten_typed_exp and" >:: flatten_and_test;
    "flatten_typed_exp not" >:: flatten_not_test;
    "flatten_typed_exp if" >:: flatten_if_test;
    "flatten_typed_exp cmp" >:: flatten_cmp_test;
    "flatten_typed_exp unop" >:: flatten_unop_test;
    "flatten_typed_exp binop" >:: flatten_binop_test;
    "flatten_typed_exp let" >:: flatten_let_test;
    "flatten_typed_exp read" >:: flatten_read_test;
  ]

let flatten_tests =
  run_test_tt_main test
