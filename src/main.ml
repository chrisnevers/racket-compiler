open Lexer
open Token
open Parser
open RProgram
open Uniquify
open Typecheck
open Flatten
open CProgram
open SelectInstructions
open AProgram
open UncoverLive
open BuildInterference

let () =
  try
    let program = 
      "(program
      (let ([v 1]) (let ([w 46]) (let ([x (+ v 7)]) 
      (let ([y (+ 4 x)]) (let ([z (+ x w)])
            (+ z (- y))))))))"
    in
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    (* print_endline "Scan"; *)
    (* print_tokens tokens; *)
    let ast = parse tokens in
    (* print_endline "\nParse"; *)
    (* print_rprogram ast; *)
    let uniq = uniquify ast in
    (* print_endline "\nUniquify"; *)
    (* print_rprogram uniq; *)
    let typed = typecheck uniq in
    (* print_endline "\nTypeCheck"; *)
    (* print_rprogram typed; *)
    let flat = flatten typed in
    (* print_endline "\nFlatten"; *)
    (* print_cprogram flat; *)
    let selins = select_instructions flat in
    (* print_endline "\nSelect Instructions"; *)
    (* print_pprogram selins; *)
    let uncovered = uncover_live selins in
    (* print_endline "\nUncover Live"; *)
    print_lprogram uncovered;
    let inter = build_interference uncovered in
    print_endline "\nBuild Interference";
    print_gprogram inter
  with ex ->
    print_endline "There was an error compiling the program:";
    print_endline (Printexc.to_string ex)
