open OUnit
open Lexer
open Parser
open Token
open RProgram
open Runners

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
  let program = "(if (and (not #f) #t) (+ 2 3) (let ([a 4]) (- a)))" in
  let tokens = run_lex program `String in
  let expected = RProgram (None, [], make_tnone (RIf
    (make_tnone (RAnd (make_tnone (RNot (make_tnone (RBool false))), make_tnone (RBool true))),
    (* then *)
    make_tnone (RBinOp ("+", make_tnone (RInt 2), make_tnone (RInt 3))),
    (* else *)
    make_tnone (RLet ("a", make_tnone (RInt 4), make_tnone (RUnOp ("-", make_tnone (RVar "a")))))))) in
  assert_equal expected (parse tokens)
)

let parse_define_test = (fun () ->
    let program = "(define (add [x : Int] [y : Int]) : Int (+ x y)) (add 1 2)" in
    let actual = run_parse program `String in
    let expected = RProgram (None, [RDefine ("add", [("x", TypeInt); ("y", TypeInt)], TypeInt,
                                    TypeIs (None, RBinOp ("+", TypeIs (None, RVar "x"), TypeIs (None, RVar "y"))))],
                                    TypeIs (None, RApply (TypeIs (None, RVar "add"), [TypeIs (None, RInt 1); TypeIs (None, RInt 2)])))
    in
    assert_equal expected actual
)

let parse_no_arg_apply_test = (fun () ->
  let program = "(foo)" in
  let actual = run_parse program `String in
  let expected = RProgram (None, [], TypeIs (None, RApply (TypeIs (None, RVar "foo"), []))) in
  assert_equal expected actual
)

let parse_vec_and_func_args_test = (fun () ->
    let program = "(define (map-vec [f : (Int -> Int)] [v : (Vector Int Int)])" ^
                  " : (Vector Int Int) (vector (f (vector-ref v 0)) (f (vector-ref v 1))))" ^
                  " (define (add1 [x : Int]) : Int (+ x 1)) (vector-ref (map-vec add1 (vector 0 41)) 1)"
    in
    let actual = run_parse program `String in
    let expected = RProgram (None,
      [RDefine ("map-vec",
      [("f", TypeFunction ([TypeInt], TypeInt));
       ("v", TypeVector [TypeInt; TypeInt])],
      TypeVector [TypeInt; TypeInt],
      TypeIs (None, RVector
        [TypeIs (None, RApply (TypeIs (None, RVar "f"), [TypeIs (None, RVectorRef (TypeIs (None, RVar "v"), 0))]));
         TypeIs (None, RApply (TypeIs (None, RVar "f"), [TypeIs (None, RVectorRef (TypeIs (None, RVar "v"), 1))]))]));
      RDefine ("add1", [("x", TypeInt)], TypeInt,
        TypeIs (None, RBinOp ("+", TypeIs (None, RVar "x"), TypeIs (None, RInt 1))))],
      TypeIs (None, RVectorRef
        (TypeIs (None, RApply (TypeIs (None, RVar "map-vec") , [TypeIs (None, RVar "add1"); TypeIs (None, RVector [TypeIs (None, RInt 0); TypeIs (None, RInt 41)])])),
        1)))
  in
  assert_equal expected actual
)

let parse_func_with_no_param_test = (fun () ->
    let program = "(define (x [y: (Int)]) : (Void) 5) 5" in
    let actual = run_parse program `String in
    let expected = RProgram (None, [RDefine ("x", [("y", TypeFunction ([], TypeInt))],
      TypeFunction ([], TypeVoid), TypeIs (None, RInt 5))], TypeIs (None, RInt 5))
    in
    assert_equal actual expected
)

let test =
  print_endline "Parser";
  "Parser" >:::
  [
    "get_token" >:: get_token_test;
    "expect_token" >:: expect_token_test;
    "parse_var" >:: parse_var_test;
    "parse" >:: parse_test;
    "parse define" >:: parse_define_test;
    "parse no arg apply" >:: parse_no_arg_apply_test;
    "parse vec and func args" >:: parse_vec_and_func_args_test;
    "parse func with no params" >:: parse_func_with_no_param_test;
  ]

let parser_tests =
  run_test_tt_main test
