open OUnit
open Uniquify
open RProgram
open Gensym
open Runners

let get_var_name_test = (fun () ->
  let v = "name" in
  let works () =
    Gensym.reset ();
    let table = Hashtbl.create 1 in
    let sigma = Hashtbl.create 1 in
    let _ = Hashtbl.add table v 1 in
    assert_equal "name1" (get_var_name v table sigma);
  in
  let throws_exn () =
    let table = Hashtbl.create 1 in
    let sigma = Hashtbl.create 1 in
    let get_var_name_fun = fun () -> get_var_name v table sigma in
    assert_raises ~msg:"Throws error if passed a token that is not TVar"
    (UniquifyError "get_var_name: Variable name is undefined") get_var_name_fun
  in
  works ();
  throws_exn ();
)

let uniquify_name_test = (fun () ->
  Gensym.reset ();
  let v = "name" in
  let table = Hashtbl.create 2 in
  assert_equal "name0" (uniquify_name v table);
  assert_equal "name1" (uniquify_name v table);
)

let uniquify_test = (fun () ->
  Gensym.reset ();
  let program = RProgram (None, [], make_tnone (RLet ("a", make_tnone (RInt 2), make_tnone (RLet ("a", make_tnone (RUnOp ("-", make_tnone (RVar "a"))), make_tnone (RVar "a")))))) in
  let expected = RProgram (None, [], make_tnone (RLet ("a0", make_tnone (RInt 2), make_tnone (RLet ("a1", make_tnone (RUnOp ("-", make_tnone (RVar "a0"))), make_tnone (RVar "a1")))))) in
  assert_equal expected (uniquify program);
)

let uniquify_def_test = (fun () ->
  Gensym.reset();
  let program = "(program (define (add [x : Int] [y : Int]) : Int (+ x y)) (add 1 2))" in
  let actual = run_uniquify program `String in
  let expected = RProgram (None, [RDefine ("add", [("x1", TypeInt); ("y2", TypeInt)], TypeInt,
    TypeIs (None, RBinOp ("+", TypeIs (None, RVar "x1"), TypeIs (None, RVar "y2"))))],
    TypeIs (None, RApply ("add", [TypeIs (None, RInt 1); TypeIs (None, RInt 2)])))
  in
  assert_equal expected actual
)

let uniquify_def2_test = (fun () ->
  Gensym.reset();
  let program = "(program (define (map-vec [f : (Int -> Int)] [v : (Vector Int Int)]) : (Vector Int Int) (vector (f (vector-ref v 0)) (f (vector-ref v 1)))) (define (add1 [x : Int]) : Int (+ x 1)) (vector-ref (map-vec add1 (vector 0 41)) 1)) " in
  let actual = run_uniquify program `String in
  let expected = RProgram (None,
  [RDefine ("map-vec",
    [("f1", TypeFunction ([TypeInt], TypeInt));
     ("v2", TypeVector [TypeInt; TypeInt])],
    TypeVector [TypeInt; TypeInt],
    TypeIs (None,
     RVector
      [TypeIs (None,
        RApply ("f1",
         [TypeIs (None, RVectorRef (TypeIs (None, RVar "v2"), 0))]));
       TypeIs (None,
        RApply ("f1",
         [TypeIs (None, RVectorRef (TypeIs (None, RVar "v2"), 1))]))]));
   RDefine ("add1", [("x4", TypeInt)], TypeInt,
    TypeIs (None,
     RBinOp ("+", TypeIs (None, RVar "x4"), TypeIs (None, RInt 1))))],
  TypeIs (None,
   RVectorRef
    (TypeIs (None,
      RApply ("map-vec",
       [TypeIs (None, RVar "add1");
        TypeIs (None, RVector [TypeIs (None, RInt 0); TypeIs (None, RInt 41)])])),
    1)))
  in
  assert_equal expected actual
)

let test =
  print_endline "Uniquify";
  "Uniquify" >:::
  [
    "get_var_name" >:: get_var_name_test;
    "uniquify_name" >:: uniquify_name_test;
    "uniquify" >:: uniquify_test;
    "uniquify def" >:: uniquify_def_test;
    "uniquify def 2" >:: uniquify_def2_test;
  ]

let uniquify_tests =
  run_test_tt_main test
