open OUnit
open Lexer
open Token

let is_digit_test = fun () ->
  assert_equal false (is_digit 'a');
  assert_equal true (is_digit '2')

let is_alpha_test = fun () ->
  assert_equal true (is_alpha 'a');
  assert_equal false (is_alpha '2')

let next_char_test = fun () ->
  let stream = Stream.of_string " a" in
  assert_equal 'a' (next_char stream)

let is_valid_id_test = fun () ->
  assert_equal true (is_valid_id 'a');
  assert_equal true (is_valid_id '2');
  assert_equal true (is_valid_id '?');
  assert_equal true (is_valid_id '_');
  assert_equal false (is_valid_id '[')

let scan_all_tokens_test = fun () ->
  let program = "()[]program let if read +- > -> >= < <= eq? and not #t #f : Int Void Vector" in
  assert_equal
    [TLParen; TRParen; TLBracket; TRBracket; TProgram; TLet; TIf; TRead;
    TArithOp "+"; TArithOp "-"; TCmpOp ">"; TArrow; TCmpOp ">="; TCmpOp "<"; TCmpOp "<=";
    TCmpOp "eq?"; TLogOp "and"; TLogOp "not"; TBool true; TBool false; TColon; TTypeInt; TTypeVoid; TTypeVector; TEOF]
    (scan_all_tokens (Stream.of_string program) [])

let scan_literal_test = fun () ->
  let stream = Stream.of_string "42" in
  assert_equal (TInt 942) (scan_literal stream "9");
  let stream = Stream.of_string "4c" in
  assert_equal (TInt 94) (scan_literal stream "9")

let scan_id_test = fun () ->
  let stream = Stream.of_string "b?" in
  assert_equal (TVar "ab?") (scan_identifier stream "a");
  let stream = Stream.of_string "rogram" in
  assert_equal TProgram (scan_identifier stream "p");
  let stream = Stream.of_string "f(" in
  assert_equal TIf (scan_identifier stream "i");
  let stream = Stream.of_string "" in
  assert_equal (TVar "a") (scan_identifier stream "a");
  let stream = Stream.of_string "q?" in
  assert_equal (TCmpOp "eq?") (scan_identifier stream "e");
  let stream = Stream.of_string "ector" in
  assert_equal TVector (scan_identifier stream "v");
  let stream = Stream.of_string "ector-ref" in
  assert_equal TVectorRef (scan_identifier stream "v");
  let stream = Stream.of_string "ector-set!" in
  assert_equal TVectorSet (scan_identifier stream "v")


let suite =
  print_endline "Lexer";
  "Lexer" >:::
  [
    "is_digit" >:: is_digit_test;
    "is_alpha" >:: is_alpha_test;
    "next_char" >:: next_char_test;
    "is_valid_id" >:: is_valid_id_test;
    "scan_literal" >:: scan_literal_test;
    "scan_id" >:: scan_id_test;
    "scan_all_tokens" >:: scan_all_tokens_test;
  ]

let lexer_tests =
  run_test_tt_main suite
