open Lexer
open Token
open Parser
open RProgram
open Expand
open Uniquify
open Monomorphification
open Typecheck
open ConvertClosures
open ExposeAllocation
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
open Gensym

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
    (* let program = "examples/pattern-match/list.rkt" in *)
    let stream = get_stream program `File in
    let tokens = scan_all_tokens stream [] in
    (* print_endline "Scan"; *)
    (* print_tokens tokens; *)
    let ast = parse tokens in
    (* print_endline "\nParse"; *)
    (* print_rprogram ast; *)
    let expanded = expand ast in
    (* print_endline "\nExpand"; *)
    (* print_rprogram expanded; *)
    let uniq = uniquify expanded in
    (* print_endline "\nUniquify"; *)
    (* print_rprogram uniq; *)
    let mono = monomorphize uniq in
    let typed = typecheck mono in
    let convert = convert_closures typed in
    (* print_endline "\nTypeCheck"; *)
    (* print_rprogram typed; *)
    let exposed = expose_allocation convert in
    (* print_endline "\nExpose"; *)
    (* print_rprogram exposed; *)
    let flat = flatten exposed in
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
    (* print_gcprogram alloc; *)
    let assignhomes = assign_homes alloc in
    (* print_endline "\nAssign Homes"; *)
    (* print_aprogram assignhomes; *)
    let lowercnd = lower_conditionals assignhomes in
    (* print_endline "\nLower Conditionals"; *)
    (* print_aprogram lowercnd; *)
    let patchinstrs = patch_instructions lowercnd in
    (* print_endline "\nPatch Instructions"; *)
    (* print_aprogram patchinstrs; *)
    let x86 = print_x86 patchinstrs in
    let filename = "output" in
    write_to_file (filename ^ ".S") x86;
    compile filename
  with ex ->
    print_endline "There was an error compiling the program:";
    print_endline (Printexc.to_string ex)
