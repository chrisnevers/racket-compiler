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
  let if_instrs = [Addq (AInt 4, AVar "1")] in
  let els_instrs = [Movq (AInt 4, AVar "y"); Addq (AInt 4, AVar "y"); Addq (AInt 5, AVar "z")] in
  let instr = AIf ((AE, AInt 1, AInt 2), if_instrs, [], els_instrs, []) in
  let expected = AIf ((AE, AInt 1, AInt 2), if_instrs, [[AVar "1"]], els_instrs, [[AVar "z"]; [AVar "y"; AVar "z"]; [AVar "z"]]) in
  let actual = uncover [instr] [] in
  assert_equal [(expected, [AVar "1"; AVar "z"])] actual
)

let uncover_while_test = (fun () ->
  let instrs = [Movq (AVoid, AVar "x0");
                AWhile ([ACallq ("read_int", [], AVar "read1"); Cmpq (AInt 0, AVar "read1");
                        Set (AG, ByteReg Al); Movzbq (ByteReg Al, AVar "cmp2")],
                        [], (AE, AInt 1, AVar "cmp2"), [Movq (AInt 5, AVar "x0")], []);
                Movq (AVar "x0", Reg Rax)]
  in
  let actual = List.rev (uncover (List.rev instrs) []) in
  let expected = [(Movq (AVoid, AVar "x0"), []);
                  (AWhile ([ACallq ("read_int", [], AVar "read1"); Cmpq (AInt 0, AVar "read1");
                        Set (AG, ByteReg Al); Movzbq (ByteReg Al, AVar "cmp2")],
                        [[AVar "x0"]; [AVar "read1"; AVar "x0"]; [AVar "x0"]; [AVar "x0"]], (AE, AInt 1, AVar "cmp2"), [Movq (AInt 5, AVar "x0")], [[]]), [AVar "x0"]);
                  (Movq (AVar "x0", Reg Rax), [AVar "x0"])]
  in
  assert_equal expected actual
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
    "uncover while test" >:: uncover_while_test;
  ]

let uncover_live_tests =
  run_test_tt_main test
