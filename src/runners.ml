open Lexer
open Token
open Parser
open RProgram
open Expand
open Uniquify
open Typecheck
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

let run_lex program input_type =
  let stream = get_stream program input_type in
  scan_all_tokens stream []

let run_parse program input_type =
  let stream = get_stream program input_type in
  let tokens = scan_all_tokens stream [] in
  parse tokens

let run_expand program input_type =
  let ast = run_parse program input_type in
  expand ast

let run_uniquify program input_type =
  let expand = run_expand program input_type in
  uniquify expand

let run_typecheck program input_type =
  let uniq = run_uniquify program input_type in
  typecheck uniq

let run_expose program input_type =
  let typed = run_typecheck program input_type in
  expose_allocation typed

let run_flatten program input_type =
  let exposed = run_expose program input_type in
  flatten exposed

let run_select_instrs program input_type =
  let flat = run_flatten program input_type in
  select_instructions flat

let run_uncover_live program input_type =
  let instr = run_select_instrs program input_type in
  uncover_live instr

let run_build_inter program input_type =
  let instr = run_uncover_live program input_type in
  build_interference instr

let run_allocate_registers program input_type =
  let instr = run_build_inter program input_type in
  allocate_registers instr

let run_assign_homes program input_type =
  let instr = run_allocate_registers program input_type in
  assign_homes instr

let run_lower_conditionals program input_type =
  let instr = run_assign_homes program input_type in
  lower_conditionals instr

let run_patch_instructions program input_type =
  let instr = run_lower_conditionals program input_type in
  patch_instructions instr

let run_print_x86 program input_type =
  let instr = run_patch_instructions program input_type in
  print_x86 instr
