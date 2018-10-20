open Token

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
  | ' ' | '\t' | '\n' -> next_char stream
  | ';' -> skip_line stream
  | c -> c

let peek_char stream : char option = Stream.peek stream

let is_valid_id c : bool =
  is_alpha c || is_digit c || c = '_' || c = '?' || c = '-' || c = '!'

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
    | "Bool"    -> TTypeBool
    | "Void"    -> TTypeVoid
    | "Vector"  -> TTypeVector
    | "lambda"  -> TLambda
    | _         -> TVar acc

let get_cmp_op c : token =
  match c with
  | 't' -> TBool true
  | 'f' -> TBool false
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
    | ':' -> TColon
    | '>' ->
      let next = peek_char stream in
      if next = Some '=' then let _ = next_char stream in TCmpOp ">=" else TCmpOp ">"
    | '<' ->
      let next = peek_char stream in
      if next = Some '=' then let _ = next_char stream in TCmpOp "<=" else TCmpOp "<"
    | '#' ->
      let next = next_char stream in
      get_cmp_op next
    | _ -> lexer_error ("scan_token: Unrecognised token: " ^ (Char.escaped c))
  with Stream.Failure -> TEOF

let rec scan_all_tokens stream tokens : token list =
  let token = scan_token stream in
  if token = TEOF then tokens @ [token]
  else scan_all_tokens stream (tokens @ [token])
