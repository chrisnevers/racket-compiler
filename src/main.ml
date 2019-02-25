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

(* Prepends the stdlib to the current program
    Outputs the result into a tmp file
 *)
let write_to_tmp streams =
  let channel = open_out "tmp.rkt" in
  List.iter (fun s ->
    Stream.iter (fun c ->
      output_char channel c
    ) s
  ) streams;
  close_out channel

let compile filename =
  let _ = Sys.command ("gcc -c " ^ filename ^ ".S -o " ^ filename ^ ".o") in
  let _ = Sys.command ("gcc " ^ filename ^ ".o" ^ " runtime/runtime.o -o " ^ filename) in
  let _ = Sys.command ("rm " ^ filename ^ ".o") in ()

let () =
  try
    let program = Sys.argv.(1) in
    (* let program = "examples/polymorphism/list.rkt" in *)
    (* Scan in stdlib *)
    let stdlib = get_stream "stdlib/stdlib.rkt" `File in
    let stdtokens = scan_all_tokens stdlib [] in
    (* Scan in program *)
    let stream = get_stream program `File in
    let tokens = scan_all_tokens stream [] in
    (* Write new file with all dependencies to tmp *)
    write_to_file "tmp.rkt" (String.concat " " (List.map string_of_token (stdtokens @ tokens)));
    (* Expand macros in tmp *)
    let expanded = MacroExpander.expand_macros "tmp.rkt" in
    write_to_file "tmp.rkt" expanded;
    (* Now compile tmp file *)
    let stream = get_stream "tmp.rkt" `File in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    let expanded = expand ast in
    let uniq = uniquify expanded in
    let mono = monomorphize uniq in
    let typed = typecheck mono in
    let convert = convert_closures typed in
    let exposed = expose_allocation convert in
    let flat = flatten exposed in
    let selinstr = select_instructions flat in
    let uncovered = uncover_live selinstr in
    let interfer = build_interference uncovered in
    let alloc = allocate_registers interfer in
    let assignhomes = assign_homes alloc in
    let lowercnd = lower_conditionals assignhomes in
    let patchinstrs = patch_instructions lowercnd in
    let x86 = print_x86 patchinstrs in
    let filename = "output" in
    write_to_file (filename ^ ".S") x86;
    compile filename
  with ex ->
    print_endline "There was an error compiling the program:";
    print_endline (Printexc.to_string ex)
