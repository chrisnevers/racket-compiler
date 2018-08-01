open OUnit
open LexerTest
open ParserTest
open UniquifyTest
open TypecheckTest
open FlattenTest
open SelectInstructionsTest

let _ =
  let _ = lexer_tests in
  let _ = parser_tests in
  let _ = uniquify_tests in
  let _ = typecheck_tests in
  let _ = flatten_tests in
  let _ = select_instructions_tests in
  ()
