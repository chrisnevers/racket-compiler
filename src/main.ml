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

let () =
  try
    let program = 
      "(program 
        (let ([a 2])
        (let ([a (- a)])
          (+ a 2))))"
    in
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    let uniq = uniquify ast in
    let typed = typecheck uniq in
    let flat = flatten typed in
    let selins = select_instructions flat in
    print_pprogram selins
  with ex ->
    print_endline "There was an error compiling the program:";
    print_endline (Printexc.to_string ex)
