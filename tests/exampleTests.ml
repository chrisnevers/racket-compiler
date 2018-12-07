open Unix

let get_input input =
  match input with Some s -> s | None -> ""

let print_error test_name input expected actual =
  print_endline (test_name ^ ": FAILED " ^ (get_input input));
  print_endline ("\tExpected result to = " ^ expected);
  print_endline ("\tBut actual result  = " ^ actual)

let print_success test_name input =
  print_endline (test_name ^ ": PASSED " ^ (get_input input))

let run_example input = try
  match input with
  | None -> input_line (Unix.open_process_in "./output")
  | Some s -> input_line (Unix.open_process_in ("echo '" ^ s ^ "' | ./output"))
  with End_of_file -> ""

let run_test test_name expected input =
  let actual = run_example input in
  match actual = expected with
  | true -> print_success test_name input
  | false -> print_error test_name input expected actual

let compile_example path =
  let _ = Sys.command ("./main.native examples/" ^ path) in ()

let compile_program () =
  let _ = Sys.command "make" in ()

let test folder name expected input =
  compile_example (folder ^ "/" ^ name ^ ".rkt");
  run_test name expected input

let () =
  compile_program ();
  print_endline "\n\nTesting Examples\n\n";
  test "basics" "add" "-10" (Some "0");
  test "basics" "neg" "-45" None;
  test "basics" "uniquify" "18" None;
  test "basics" "mult" "192000" (Some "3");
  test "basics" "div" "35" None;
  test "basics" "modulo" "21" None;
  test "reg-alloc" "liveness" "42" None;
  test "control-flow" "and" "#t" (Some "10");
  test "control-flow" "and" "#f" (Some "5");
  test "control-flow" "or" "#t" (Some "10");
  test "control-flow" "or" "#t" (Some "0");
  test "control-flow" "or" "#f" (Some "5");
  test "control-flow" "not" "#f" None;
  test "control-flow" "eq" "#t" (Some "10");
  test "control-flow" "eq" "#f" (Some "5");
  test "control-flow" "gt" "#t" (Some "15");
  test "control-flow" "gt" "#f" (Some "10");
  test "control-flow" "gte" "#t" (Some "10");
  test "control-flow" "gte" "#f" (Some "5");
  test "control-flow" "if" "30" (Some "20");
  test "control-flow" "if" "0" (Some "10");
  test "control-flow" "lt" "#t" (Some "5");
  test "control-flow" "lt" "#f" (Some "10");
  test "control-flow" "lte" "#t" (Some "10");
  test "control-flow" "lte" "#f" (Some "15");
  test "control-flow" "pos?" "#t" None;
  test "control-flow" "neg?" "#t" None;
  test "control-flow" "zero?" "#t" (Some "0");
  test "control-flow" "zero?" "#f" (Some "5");
  test "control-flow" "begin" "2" (Some "5");
  test "control-flow" "while" "" (Some "5");
  test "control-flow" "while" "0" (Some "0");
  test "control-flow" "while2" "0" (Some "0");
  test "control-flow" "while2" "5" (Some "5\n0");
  test "heap" "print_int" "50" None;
  test "heap" "print_bool" "#t" None;
  test "heap" "print_void" "" None;
  test "heap" "vector" "(1, 2, 3)" None;
  test "heap" "vector-ref" "#t" None;
  test "heap" "vector-set" "15" None;
  test "heap" "nested-vector" "(((1, 2), 3), (1, 2))" None;
  test "heap" "call_collect" "((1, 2, 3), (1, 2, 3))" None;
  test "heap" "nested-vec-ref" "42" None;
  test "heap" "vec-length" "2" None;
  test "functions" "add" "3" None;
  test "functions" "map-vec" "(6, 10)" None;
  (* This only captures first line of output *)
  test "functions" "recursion" "10" None;
  test "functions" "print-type" "((Int -> Bool) -> (Int * Bool) -> Void)" None;
  test "functions" "no-args" "(Void)" None;
  test "functions" "mutually-recursive" "5" None;
  test "functions" "ex" "5" None;
  test "functions" "save-atomics" "28" None;
  test "functions" "save-ptrs" "28" None;
  test "closures" "lambda" "20" None;
  test "closures" "book" "42" None;
  test "closures" "print-lambda" "((Int * Int) -> Bool)" None;
  test "arrays" "init" "#[#[1, 2], #[3, 4], #[5, 6]]" None;
  test "arrays" "array-arg" "((Array (Array Int)) -> (Array Int))" None;
  test "arrays" "array-set" "#[#[1, 2, 3], #[42]]" None;
  test "arrays" "array-ref" "1" (Some "0");
  test "char" "char" "#[a,  , c]" None;
  test "pattern-match" "case" "#t" None;
  test "pattern-match" "print" "(Booly #f)" None;
  test "pattern-match" "print-type" "(ascii -> Void)" None;
  test "pattern-match" "list" "(Cons (4, (Cons (5, (Cons (6, (Nil #f)))))))" None;
  test "pattern-match" "interp" "5" None;
