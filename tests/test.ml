open OUnit
open LexerTest
open ParserTest
open UniquifyTest

let _ =
  let _ = lexer_tests in
  let _ = parser_tests in
  let _ = uniquify_tests in
  ()
