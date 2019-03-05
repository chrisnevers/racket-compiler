open Token
open Helper

exception LexerError of string

let lexer_error s = raise (LexerError s)

(* is character a digit? *)
let is_digit c : bool =
  let code = Char.code c in
  code >= Char.code('0') && code <= Char.code('9')

(* is character alphanumerical? *)
let is_alpha c : bool =
  let code = Char.code c in
  (code >= Char.code('A') && code <= Char.code('Z')) ||
  (code >= Char.code('a') && code <= Char.code('z'))

let is_space c = c = ' ' || c = '\n' || c = '\t'

(* Construct a stream from a file or a string. Valid types are File or String *)
let get_stream src stream_type : char Stream.t =
  match stream_type with
  | `File ->
    let channel = open_in src in
    let stream = Stream.of_channel channel in stream
    (* let _ = close_in channel in stream *)
  | `String -> Stream.of_string src

let rec skip_line stream =
  let nc = Stream.next stream in
  match nc with
  | '\n' -> next_char stream
  | _ -> skip_line stream

(* Skips white space *)
and next_char stream : char =
  let nc = Stream.next stream in
  match nc with
  | s when is_space nc -> next_char stream
  | ';' -> skip_line stream
  | c -> c

and next_char_with_space stream : char = Stream.next stream

let peek_char stream : char option = Stream.peek stream

let is_valid_id c : bool =
  is_alpha c || is_digit c || c = '_' || c = '?' || c = '-' || c = '!' || c = '>' || c = '='

let is_stream_empty stream : bool =
  try Stream.empty stream; true
  with Stream.Failure -> false

let rec scan_literal stream acc : token =
  let next = peek_char stream in
  match next with
  | Some c when is_digit c ->
    let _ = next_char stream in
    scan_literal stream (acc ^ (Char.escaped c))
  | _ -> TInt (int_of_string acc)

let rec scan_identifier stream acc : token =
  let next = peek_char stream in
  match next with
  | Some c when is_valid_id c ->
    let _ = next_char stream in
    scan_identifier stream (acc ^ (Char.escaped c))
  | _ ->
    match acc with
    | "program" -> TProgram
    | "read"    -> TRead
    | "let"     -> TLet
    | "if"      -> TIf
    | "and"     -> TLogOp "and"
    | "or"      -> TLogOp "or"
    | "not"     -> TLogOp "not"
    | "eq?"     -> TCmpOp "eq?"
    | "pos?"    -> TPos
    | "neg?"    -> TNeg
    | "zero?"   -> TZero
    | "void"    -> TVoid
    | "array"   -> TArray
    | "array-set!" -> TArraySet
    | "array-ref"  -> TArrayRef
    | "vector"  -> TVector
    | "vector-set!" -> TVectorSet
    | "vector-ref"  -> TVectorRef
    | "vector-length" -> TVectorLength
    | "begin"   -> TBegin
    | "when"    -> TWhen
    | "unless"  -> TUnless
    | "print"   -> TPrint
    | "while"   -> TWhile
    | "define"  -> TDefine
    | "Int"     -> TTypeInt
    | "Char"    -> TTypeChar
    | "Bool"    -> TTypeBool
    | "Void"    -> TTypeVoid
    | "Array"   -> TTypeArray
    | "Vector"  -> TTypeVector
    | "Forall"  -> TTypeForAll
    | "Fix"     -> TTypeFix
    | "lambda"  -> TLambda
    | "Lambda"  -> TTyLambda
    | "inst"    -> TInst
    | "define-type" -> TDefineType
    | "case"    -> TCase
    | "inl"     -> TInl
    | "inr"     -> TInr
    | "inl?"    -> TIsInl
    | "inr?"    -> TIsInr
    | ":"       -> TColon
    | "import"  ->
      let filename = scan_string stream in
      TImport filename
    | _         -> TVar acc

and scan_string stream =
  let rec _helper acc is_open =
  match next_char stream with
  | '"' -> if is_open then acc
    else _helper acc true
  | c -> _helper (acc ^ (Char.escaped c)) is_open
  in
  _helper "" false

let is_closing c = c = ')' || c = ']'
let is_char_delim c = is_space c || is_closing c

let rec scan_char acc stream =
  let next = peek_char stream in
  match next with
  | Some c when not (is_char_delim c) ->
    let _ = next_char stream in
    scan_char (acc ^ (Char.escaped c)) stream
  | _ -> match acc with
    | "space"   -> TChar ' '
    | "newline" -> TChar '\n'
    | "tab"     -> TChar '\t'
    | _ -> match String.length acc with
      | 1 -> TChar acc.[0]
      | _ -> lexer_error "unexpected char sequence"


let lex_hash c stream : token =
  match c with
  | 't' -> TBool true
  | 'f' -> TBool false
  | '\\' -> try
    let c = next_char_with_space stream in
    if is_closing c then TChar c
    else scan_char (Char.escaped c) stream
    with Stream.Failure -> lexer_error "expected character after #\\"
  | _ -> lexer_error ("scan_token: Expected #t or #f but received #" ^ (Char.escaped c))

let scan_token stream : token = try
  match is_stream_empty stream with
  | true -> TEOF
  | false ->
    let c = next_char stream in
    if is_alpha c then scan_identifier stream (Char.escaped c)
    else if is_digit c then scan_literal stream (Char.escaped c)
    else match c with
    | '*' -> TArithOp "*"
    | '/' -> TArithOp "/"
    | '%' -> TArithOp "%"
    | '+' -> TArithOp "+"
    | '-' ->
      let next = peek_char stream in
      if next = Some '>' then
        let _ = next_char stream in TArrow
      else TArithOp "-"
    | '(' -> TLParen
    | ')' -> TRParen
    | '[' -> TLBracket
    | ']' -> TRBracket
    | '_' -> TVar "_"
    | '>' ->
      let next = peek_char stream in
      if next = Some '=' then let _ = next_char stream in TCmpOp ">=" else TCmpOp ">"
    | '<' ->
      let next = peek_char stream in
      if next = Some '=' then let _ = next_char stream in TCmpOp "<=" else TCmpOp "<"
    | '#' ->
      let next = next_char stream in
      lex_hash next stream
    | '.' -> TDot
    | ow -> scan_identifier stream (Char.escaped ow)
  with Stream.Failure -> TEOF

let imported = ref []

let rec scan_all_tokens stream tokens : token list =
  let token = scan_token stream in
  if token = TEOF then
    import (tokens @ [token])
  else scan_all_tokens stream (tokens @ [token])

and import tokens =
  let open List in
  match tokens with
  | TLParen :: TImport name :: TRParen :: tl when not (mem name !imported) ->
    (* print_endline ("Importing dependency:  " ^ name); *)
    imported := name :: !imported;
    let file_tokens = scan_all_tokens (get_stream (name ^ ".rkt") `File) [] in
    import (file_tokens @ tl)
  | TLParen :: TImport name :: TRParen :: tl when mem name !imported ->
    (* print_endline ("Dependency already included"); *)
    import tl
  | ow :: tl -> ow :: import tl
  | [] -> []
