open OUnit
open Parser
open Token
open RProgram

let get_token_test = (fun () ->
  let tokens = [TLParen; TProgram] in
  let tokens_list = ref tokens in
  let token = get_token tokens_list in
  assert_equal TLParen token;
  assert_equal TProgram (next_token tokens_list);
)

let expect_token_test = (fun () ->
  let tokens = [TLParen; TProgram] in
  let tokens_list = ref tokens in
  let works () =
    assert_equal () (expect_token tokens_list TLParen)
  in
  let throws_exn () =
    let expect_token_fun = fun () -> expect_token tokens_list TRBracket in
    assert_raises ~msg:"Throws error if expecting wrong type of token"
      (ParserError "Expected ] but received Program") expect_token_fun
  in
  works ();
  throws_exn ();
)

let parse_var_test = (fun () ->
  let tokens = ref [TVar "name"] in
  let works () = assert_equal "name" (parse_var tokens) in
  let throws_exn () =
    let tokens = ref [TInt 4] in
    let parse_var_fun = fun () -> parse_var tokens in
    assert_raises ~msg:"Throws error if passed a token that is not TVar"
    (ParserError "Expected var but received Int 4") parse_var_fun
  in
  works ();
  throws_exn ();
)

let parse_test = (fun () ->
  (* let program = "(program (if (and (not #f) #t) (+ 2 3) (let ([a 4]) (- a))))" in *)
  let tokens = [TLParen; TProgram; TLParen; TIf; TLParen; TLogOp "and"; TLParen;
    TLogOp "not"; TBool false; TRParen; TBool true; TRParen; TLParen;
    TArithOp "+"; TInt 2; TInt 3; TRParen; TLParen; TLet; TLParen; TLBracket;
    TVar "a"; TInt 4; TRBracket; TRParen; TLParen; TArithOp "-"; TInt 4;
    TRParen; TRParen; TRParen; TRParen; TEOF]
  in
  let expected = RProgram (None, make_tnone (RIf
    (make_tnone (RAnd (make_tnone (RNot (make_tnone (RBool false))), make_tnone (RBool true))),
    (* then *)
    make_tnone (RBinOp ("+", make_tnone (RInt 2), make_tnone (RInt 3))),
    (* else *)
    make_tnone (RLet ("a", make_tnone (RInt 4), make_tnone (RUnOp ("-", make_tnone (RInt 4)))))))) in
  assert_equal expected (parse tokens)
)

let test =
  print_endline "Parser";
  "Parser" >:::
  [
    "get_token" >:: get_token_test;
    "expect_token" >:: expect_token_test;
    "parse_var" >:: parse_var_test;
    "parse" >:: parse_test;
  ]

let parser_tests =
  run_test_tt_main test
