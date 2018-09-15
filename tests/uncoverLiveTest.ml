open OUnit
open UncoverLive
open AProgram
open Helper
open List

let get_var_list_or_empty_test = (fun () ->
  assert_equal [AVar "v"] (get_var_list_or_empty (AVar "v"));
  assert_equal [] (get_var_list_or_empty (AInt 5));
)

let get_written_vars_test = (fun () ->
  assert_equal [AVar "v"] (get_written_vars (Movq (AVar "x", AVar "v")));
  assert_equal [AVar "v"] (get_written_vars (Addq (AVar "x", AVar "v")));
  assert_equal [AVar "v"] (get_written_vars (Subq (AVar "x", AVar "v")));
  assert_equal [AVar "v"] (get_written_vars (Movzbq (AVar "x", AVar "v")));
  assert_equal [AVar "v"] (get_written_vars (Xorq (AVar "x", AVar "v")));
  assert_equal [AVar "v"] (get_written_vars (Set (AE, AVar "v")));
  assert_equal [AVar "v"] (get_written_vars (Negq (AVar "v")));
)


let get_read_vars_test = (fun () ->
  assert_equal [AVar "x"; AVar "v"] (get_read_vars (Addq (AVar "x", AVar "v")));
  assert_equal [AVar "x"; AVar "v"] (get_read_vars (Subq (AVar "x", AVar "v")));
  assert_equal [AVar "x"; AVar "v"] (get_read_vars (Cmpq (AVar "x", AVar "v")));
  assert_equal [AVar "x"; AVar "v"] (get_read_vars (Xorq (AVar "x", AVar "v")));
  assert_equal [AVar "x"] (get_read_vars (Movq (AVar "x", AVar "v")));
  assert_equal [AVar "x"] (get_read_vars (Movzbq (AVar "x", AVar "v")));
  (* assert_equal caller_save_aregisters (get_read_vars (Callq "label")); *)
)

let uncover_stmt_test = (fun () ->
  assert_equal [(Movq (AInt 4, AVar "v"), [])] (uncover [Movq (AInt 4, AVar "v")] [AVar "v"]);
  assert_equal [(Movq (AInt 4, AVar "v"), [])] (uncover [Movq (AInt 4, AVar "v")] []);
  assert_equal [(Movq (AInt 4, AVar "v"), []); (Addq (AInt 4, AVar "v"), [AVar "v"])] (uncover [Movq (AInt 4, AVar "v"); Addq (AInt 4, AVar "v")] []);
)

let uncover_if_test = (fun () ->
  let if_instrs = [Movq (AInt 3, AVar "1"); Addq (AInt 4, AVar "1")] in
  let els_instrs = [Movq (AInt 4, AVar "2"); Addq (AInt 4, AVar "2")] in
  let instr = AIf ((AE, AInt 1, AInt 2), if_instrs, [], els_instrs, []) in
  let expected = AIf ((AE, AInt 1, AInt 2), if_instrs, [[]; [AVar "1"]], els_instrs, [[]; [AVar "2"]]) in
  let actual = (uncover [instr] []) in
  assert_equal [(expected, [AVar "1"; AVar "2"])] actual
)

let test =
  print_endline "Uncover Live";
  "Uncover Live" >:::
  [
    "get_var_list_or_empty" >:: get_var_list_or_empty_test;
    "get_written_vars" >:: get_written_vars_test;
    "get_read_vars" >:: get_read_vars_test;
    "uncover stmt test" >:: uncover_stmt_test;
    "uncover if test" >:: uncover_if_test;
  ]

let uncover_live_tests =
  run_test_tt_main test
