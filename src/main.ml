open Lexer
open Token
open Parser
open RProgram
open Expand
open Uniquify
open Typecheck
open Flatten
open CProgram
open SelectInstructions
open AProgram
open UncoverLive
open BuildInterference
open AllocateRegisters
open LowerConditionals
open AssignHomes
open PatchInstructions
open Printx86

let write_to_file file str =
  let channel = open_out file in
  output_string channel str;
  close_out channel

let compile filename =
  let _ = Sys.command ("gcc -c " ^ filename ^ ".S -o " ^ filename ^ ".o") in
  let _ = Sys.command ("gcc " ^ filename ^ ".o" ^ " runtime/runtime.o -o " ^ filename) in
  let _ = Sys.command ("rm " ^ filename ^ ".o") in ()

let () =
  try
    let program = Sys.argv.(1) in
    let stream = get_stream program `File in
    let tokens = scan_all_tokens stream [] in
    (* print_endline "Scan"; *)
    (* print_tokens tokens; *)
    let ast = parse tokens in
    let expanded = expand ast in
    (* print_endline "\nParse"; *)
    (* print_rprogram ast; *)
    let uniq = uniquify expanded in
    (* print_endline "\nUniquify"; *)
    (* print_rprogram uniq; *)
    let typed = typecheck uniq in
    (* print_endline "\nTypeCheck"; *)
    (* print_rprogram typed; *)
    let flat = flatten typed in
    (* print_endline "\nFlatten"; *)
    (* print_cprogram flat; *)
    let selinstr = select_instructions flat in
    (* print_endline "\nSelect Instructions"; *)
    (* print_pprogram selinstr; *)
    let uncovered = uncover_live selinstr in
    (* print_endline "\nUncover Live"; *)
    (* print_lprogram uncovered; *)
    let interfer = build_interference uncovered in
    (* print_endline "\nBuild Interference"; *)
    (* print_gprogram interfer; *)
    let alloc = allocate_registers interfer in
    (* print_endline "\nAllocate Registers"; *)
    (* print_gprogram alloc; *)
    let lowercnd = lower_conditionals alloc in
    (* print_endline "\nLower Conditionals"; *)
    (* print_gprogram lowercnd; *)
    let assignhomes = assign_homes lowercnd in
    (* print_endline "\nAssign Homes";
    print_gprogram asshomes; *)
    let patchinstrs = patch_instructions assignhomes in
    (* print_endline "\nPatch Instructions";
    print_aprogram patchi; *)
    let x86 = print_x86 patchinstrs in
    let filename = "output" in
    write_to_file (filename ^ ".S") x86;
    compile filename
  with ex ->
    print_endline "There was an error compiling the program:";
    print_endline (Printexc.to_string ex)
