open OUnit
open Uniquify
open RProgram

let get_var_name_test = (fun () ->
  let v = "name" in
  let works () =
    let table = Hashtbl.create 1 in
    let _ = Hashtbl.add table v 1 in
    assert_equal "name" (get_var_name v table);
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
  let v = "name" in
  let table = Hashtbl.create 2 in
  assert_equal "name" (uniquify_name v table);
  assert_equal "name2" (uniquify_name v table);
)

let uniquify_test = (fun () ->
  let program = RProgram (TypeVoid, make_tint (RLet ("a", make_tint (RInt 2), make_tint (RLet ("a", make_tint (RUnOp ("-", make_tint (RVar "a"))), make_tint (RVar "a")))))) in
  let expected = RProgram (TypeVoid, make_tint (RLet ("a", make_tint (RInt 2), make_tint (RLet ("a2", make_tint (RUnOp ("-", make_tint (RVar "a"))), make_tint (RVar "a2")))))) in
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
