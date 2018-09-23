open OUnit
open Uniquify
open RProgram
open Gensym

let get_var_name_test = (fun () ->
  let v = "name" in
  let works () =
    Gensym.reset ();
    let table = Hashtbl.create 1 in
    let _ = Hashtbl.add table v 1 in
    assert_equal "name1" (get_var_name v table);
  in
  let throws_exn () =
    let table = Hashtbl.create 1 in
    let get_var_name_fun = fun () -> get_var_name v table in
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
  let program = RProgram (None, make_tnone (RLet ("a", make_tnone (RInt 2), make_tnone (RLet ("a", make_tnone (RUnOp ("-", make_tnone (RVar "a"))), make_tnone (RVar "a")))))) in
  let expected = RProgram (None, make_tnone (RLet ("a0", make_tnone (RInt 2), make_tnone (RLet ("a1", make_tnone (RUnOp ("-", make_tnone (RVar "a0"))), make_tnone (RVar "a1")))))) in
  assert_equal expected (uniquify program);
)

let test =
  print_endline "Uniquify";
  "Uniquify" >:::
  [
    "get_var_name" >:: get_var_name_test;
    "uniquify_name" >:: uniquify_name_test;
    "uniquify" >:: uniquify_test;
  ]

let uniquify_tests =
  run_test_tt_main test
