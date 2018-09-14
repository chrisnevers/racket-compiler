open OUnit
open Typecheck
open RProgram

let get_var_type_test = (fun () ->
  let v = "name" in
  let works () = 
    let table = Hashtbl.create 1 in
    let _ = Hashtbl.add table v TypeInt in
    assert_equal TypeInt (get_var_type v table);
  in
  let throws_exn () =
    let table = Hashtbl.create 1 in
    let get_var_type_fun = fun () -> get_var_type v table in
    assert_raises ~msg:"Throws error if passed a variable that is not defined"
    (TypecheckError "get_var_type: Undeclared variable") get_var_type_fun
  in
  works ();
  throws_exn ();
)

let typecheck_exp_and_test = (fun () ->
  let table = Hashtbl.create 1 in
  let works () =
    let exp = RAnd (RBool true, RBool false) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let throws_exn () =
    let exp = RAnd (RBool true, RInt 0) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if passed non bool"
    (TypecheckError "typecheck_exp: And expressions must operate on boolean values") typecheck_exp_fun
  in
  works ();
  throws_exn ();
)

let typecheck_exp_not_test = (fun () ->
  let table = Hashtbl.create 1 in
  let works () =
    let exp = RNot (RBool true) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let throws_exn () =
    let exp = RNot (RInt 3) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if passed non bool"
    (TypecheckError "typecheck_exp: Not expressions must operate on boolean values") typecheck_exp_fun
  in
  works ();
  throws_exn ();
)

let typecheck_exp_if_test = (fun () ->
  let table = Hashtbl.create 1 in
  let works () =
    let exp = RIf (RCmp ("<", RInt 4, RInt 5), RBool true, RBool false) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let throws_exn_cnd () =
    let exp = RIf (RInt 4, RBool true, RBool false) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if condition does not eval to bool"
    (TypecheckError  "typecheck_exp: If condition must evaluate to boolean value") typecheck_exp_fun
  in
  let throws_exn_type () =
    let exp = RIf (RCmp ("<", RInt 4, RInt 5), RBool true, RInt 4) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if then and else don't evaluate to same type"
    (TypecheckError  "typecheck_exp: If condition's then and else must evaluate to same type") typecheck_exp_fun
  in
  works ();
  throws_exn_cnd ();
  throws_exn_type ();
)

let typecheck_exp_cmp_test = (fun () ->
  let table = Hashtbl.create 1 in
  let lt_works () =
    let exp = RCmp ("<", RInt 4, RInt 5) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let lte_works () =
    let exp = RCmp ("<=", RInt 4, RInt 5) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let gt_works () =
    let exp = RCmp (">", RInt 4, RInt 5) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let gte_works () =
    let exp = RCmp (">=", RInt 4, RInt 5) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let eq_works () =
    let exp = RCmp ("eq?", RInt 4, RInt 5) in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  let throws_exn_type () =
    let exp = RCmp ("<", RBool true, RBool false) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if arith cmp applied on wrong type"
    (TypecheckError  "typecheck_exp: < operates on integers") typecheck_exp_fun
  in
  let throws_exn_eq () =
    let exp = RCmp ("eq?", RBool false, RInt 5) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if eq? applied on different types"
    (TypecheckError  "typecheck_exp: eq? only compares same type") typecheck_exp_fun
  in
  lt_works ();
  lte_works ();
  gt_works ();
  gte_works ();
  eq_works ();
  throws_exn_type ();
  throws_exn_eq ();
)

let typecheck_exp_unop_test = (fun () ->
  let table = Hashtbl.create 1 in
  let works () =
    let exp = RUnOp ("-", RInt 4) in
    assert_equal TypeInt (typecheck_exp exp table)
  in
  let throws_exn () =
    let exp = RUnOp ("-", RBool false) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if wrong type"
    (TypecheckError "typecheck_exp: - must be applied on integer") typecheck_exp_fun
  in
  works ();
  throws_exn ();
)

let typecheck_exp_binop_test = (fun () ->
  let table = Hashtbl.create 1 in
  let works () =
    let exp = RBinOp ("-", RInt 4, RInt 5) in
    assert_equal TypeInt (typecheck_exp exp table)
  in
  let throws_exn () =
    let exp = RBinOp ("+", RInt 4, RBool false) in
    let typecheck_exp_fun = fun () -> typecheck_exp exp table in
    assert_raises ~msg:"Throws error if wrong type"
    (TypecheckError "typecheck_exp: + must be applied on integers") typecheck_exp_fun
  in
  works ();
  throws_exn ();
)

let typecheck_exp_let_test = (fun () ->
  let table = Hashtbl.create 1 in
  let works_int () =
    let exp = RLet ("v", RInt 4, RVar "v") in
    assert_equal TypeInt (typecheck_exp exp table)
  in
  let works_bool () =
    let exp = RLet ("v", RBool false, RVar "v") in
    assert_equal TypeBool (typecheck_exp exp table)
  in
  works_int ();
  works_bool ();
)

let test =
  print_endline "Typecheck";
  "Typecheck" >:::
  [
    "get_var_type" >:: get_var_type_test;
    "typecheck_exp and" >:: typecheck_exp_and_test;
    "typecheck_exp not" >:: typecheck_exp_not_test;
    "typecheck_exp if" >:: typecheck_exp_if_test;
    "typecheck_exp cmp" >:: typecheck_exp_cmp_test;
    "typecheck_exp unop" >:: typecheck_exp_unop_test;
    "typecheck_exp binop" >:: typecheck_exp_binop_test;
    "typecheck_exp let" >:: typecheck_exp_let_test;
  ]

let typecheck_tests =
  run_test_tt_main test