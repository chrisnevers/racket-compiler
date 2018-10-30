open OUnit
open BuildInterference
open RProgram
open AProgram
open Registers
open List

let find_in_map_test = (fun () ->
  (* Not found *)
  let tbl = Hashtbl.create 2 in
  let res = find_in_map ("s") tbl in
  assert_equal [] res;
  (* Found *)
  let value = [AVar "x"] in
  Hashtbl.add tbl "s" value;
  let res = find_in_map ("s") tbl in
  assert_equal value res
)

let append_to_value_test = (fun () ->
  let tbl = Hashtbl.create 2 in
  Hashtbl.add tbl "s" [AVar "b"; AVar "c"];
  append_to_value "s" (AVar "a") tbl;
  let expected = [AVar "a"; AVar "b"; AVar "c"] in
  let actual = Hashtbl.find tbl "s" in
  assert_equal expected actual
)

let add_bidirected_edges_if_test = (fun () ->
  let cnd_true () =
    let tbl = Hashtbl.create 2 in
    let d = AVar "a" in
    let target = AVar "b" in
    add_bidirected_edges_if (fun e -> true) d [target] tbl;
    assert_equal [AVar "b"] (Hashtbl.find tbl d);
    assert_equal [AVar "a"] (Hashtbl.find tbl target);
  in
  let cnd_false () =
    let tbl = Hashtbl.create 2 in
    let d = AVar "a" in
    let target = AVar "b" in
    Hashtbl.add tbl d [];
    Hashtbl.add tbl target [];
    add_bidirected_edges_if (fun e -> false) d [target] tbl;
    assert_equal [] (Hashtbl.find tbl d);
    assert_equal [] (Hashtbl.find tbl target);
  in
  cnd_true ();
  cnd_false ()
)

let add_directed_edges_test = (fun () ->
  let n1 = AVar "a" in
  let n2 = AVar "b" in
  let n3 = AVar "c" in
  let nodes = [n1; n2] in
  let targets = [n3] in
  let tbl = Hashtbl.create 3 in
  Hashtbl.add tbl n3 [];
  add_directed_edges nodes targets tbl;
  assert_equal [n3] (Hashtbl.find tbl n1);
  assert_equal [n3] (Hashtbl.find tbl n2);
  assert_equal [] (Hashtbl.find tbl n3);
)

let get_live_vectors_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    Hashtbl.add var_types "s" TypeInt;
    Hashtbl.add var_types "x" (TypeVector [TypeInt]);
    let res = get_live_vectors [AVar "s"; AVar "x"] var_types in
    assert_equal [AVar "x"] res
)

let build_graph_movq_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    let map = Hashtbl.create 2 in
    Hashtbl.add map (AVar "c") [];
    let live = [[AVar "a"; AVar "b"; AVar "c"]] in
    let stmt = Movq (AVar "c", AVar "b") in
    let map = build_graph [stmt] live map var_types in
    assert_equal [AVar "b"] (Hashtbl.find map (AVar "a"));
    assert_equal [AVar "a"] (Hashtbl.find map (AVar "b"));
    assert_equal [] (Hashtbl.find map (AVar "c"))
)

let build_graph_addq_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    let map = Hashtbl.create 2 in
    let live = [[AVar "a"; AVar "b"; AVar "c"]] in
    let stmt = Addq (AVar "c", AVar "b") in
    let map = build_graph [stmt] live map var_types in
    assert_equal [AVar "b"] (Hashtbl.find map (AVar "a"));
    assert_equal [AVar "b"] (Hashtbl.find map (AVar "c"));
    assert_equal [AVar "a"; AVar "c"] (Hashtbl.find map (AVar "b"))
)

let build_graph_mulq_test = (fun () ->
  let var_types = Hashtbl.create 2 in
  let map = Hashtbl.create 2 in
  let live = [[AVar "a"; AVar "b"; AVar "c"]] in
  let stmt = IMulq (AVar "c", AVar "b") in
  let map = build_graph [stmt] live map var_types in
  assert_equal [AVar "b"; Reg Rdx] (Hashtbl.find map (AVar "a"));
  assert_equal [AVar "b"; Reg Rdx] (Hashtbl.find map (AVar "c"));
  assert_equal [AVar "a"; AVar "c"; Reg Rdx] (Hashtbl.find map (AVar "b"));
)

let build_graph_divq_test = (fun () ->
  let var_types = Hashtbl.create 2 in
  let map = Hashtbl.create 2 in
  let live = [[AVar "a"; AVar "b"; AVar "c"]] in
  let stmt = IDivq (AVar "a") in
  let map = build_graph [stmt] live map var_types in
  assert_equal [Reg Rdx] (Hashtbl.find map (AVar "a"));
  assert_equal [Reg Rdx] (Hashtbl.find map (AVar "c"));
  assert_equal [Reg Rdx] (Hashtbl.find map (AVar "b"));
)

let build_graph_callq_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    let map = Hashtbl.create 2 in
    let live = [[AVar "a"]] in
    let stmt = Callq ("hey") in
    let map = build_graph [stmt] live map var_types in
    assert_equal caller_save_aregisters (Hashtbl.find map (AVar "a"));
)

let build_graph_acallq_general_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    let map = Hashtbl.create 2 in
    let live = [[AVar "a"]] in
    let stmt = ACallq (GlobalValue "hey", [], AVar "b") in
    let map = build_graph [stmt] live map var_types in
    assert_equal (AVar "b" :: caller_save_aregisters) (Hashtbl.find map (AVar "a"));
    assert_equal [AVar "a"] (Hashtbl.find map (AVar "b"));
)

let build_graph_acallq_collect_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    let map = Hashtbl.create 2 in
    Hashtbl.add var_types "a" (TypeVector [TypeInt]);
    let live = [[AVar "a"]] in
    let stmt = ACallq (GlobalValue "collect", [], AVar "_") in
    let map = build_graph [stmt] live map var_types in
    let res = Hashtbl.find map (AVar "a") in
    assert_equal callee_save_aregisters res;
)

let build_graph_acallq_var_test = (fun () ->
    let var_types = Hashtbl.create 2 in
    let map = Hashtbl.create 2 in
    let live = [[AVar "l"]] in
    let stmt = ACallq (AVar "a", [AVar "b"; AVar "c"], AVar "d") in
    let map = build_graph [stmt] live map var_types in
    let actual = Hashtbl.find map (AVar "l") in
    let expected = [AVar "a"; AVar "b"; AVar "c"; AVar "d"] @ caller_save_aregisters in
    assert_equal expected actual;
)

let build_defs_test = (fun () ->
  let instrs = [Addq (AInt 1, AVar "x")] in
  let lives = [[AVar "x"]] in
  let types = Hashtbl.create 1 in
  Hashtbl.add types "x" TypeInt;
  let defs = [LDefine ("foo", 1, [AVar "x"], types, 0, lives, instrs)] in
  let actual = hd (build_defs defs) in
  match actual with
  | GDefine (id, param, args, types, stack, lives, map, instrs) ->
    let arg_inter = Hashtbl.find map (AVar "x") in
    assert_equal callee_save_aregisters arg_inter
)

let test =
  print_endline "Build Interference";
  "Build Interference" >:::
  [
    "find_in_map test" >:: find_in_map_test;
    "append_to_value test" >:: append_to_value_test;
    "add_bidirected_edges_if test" >:: add_bidirected_edges_if_test;
    "add_directed_edges test" >:: add_directed_edges_test;
    "get_live_vectors test" >:: get_live_vectors_test;
    "build_graph_movq test" >:: build_graph_movq_test;
    "build_graph_addq test" >:: build_graph_addq_test;
    "build_graph_divq test" >:: build_graph_divq_test;
    "build_graph_mulq test" >:: build_graph_mulq_test;
    "build_graph_callq test" >:: build_graph_callq_test;
    "build_graph_acallq_general test" >:: build_graph_acallq_general_test;
    "build_graph_acallq_collect test" >:: build_graph_acallq_collect_test;
    "build_graph_acallq_var test" >:: build_graph_acallq_var_test;
    "build_defs test" >:: build_defs_test;
  ]

let build_interference_tests =
  run_test_tt_main test