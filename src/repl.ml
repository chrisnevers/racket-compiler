open Lexer
open Token
open Parser
open RProgram
open Uniquify

exception UnsupportedOperator of string
let unsupported_operator s = raise (UnsupportedOperator s)

exception VariableNotFound of string
let variable_not_found s = raise (VariableNotFound s)

let rec count_char c str count =
  match str with
  | [] -> count
  | h :: t when h = c -> count_char c t (count + 1)
  | h :: t -> count_char c t count

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec get_input acc opened_parens =
  let line = read_line() in
  let exploded = explode line in
  let open_parens = count_char '(' exploded opened_parens in
  let closed_parens = count_char ')' exploded 0 in
  let currently_open = open_parens - closed_parens in
  if currently_open > 0 then
    (print_string "  "; get_input (acc ^ line) currently_open)
  else (acc ^ line)

let int_of_bool (b:bool) : int =
  match b with
  | true -> 1
  | false -> 0

let bool_of_int (b:int) : bool =
  match b with
  | 0 -> false
  | _ -> true

let rec evaluate ast table =
  match ast with
  | RVar v ->
    (try
      Hashtbl.find table v
    with Not_found -> variable_not_found ("Variable: " ^ v ^ " used before declaration"))
  | RInt i -> i
  | RBool b -> int_of_bool b
  | RAnd (l, r) ->
    let lexp = evaluate l table in
    let rexp = evaluate r table in
    (int_of_bool (lexp = 1 && rexp = 1))
  | ROr (l, r) ->
    let lexp = evaluate l table in
    let rexp = evaluate r table in
    (int_of_bool (lexp = 1 || rexp = 1))
  | RNot e ->
    let exp = evaluate e table in
    (int_of_bool (not (bool_of_int exp)))
  | RIf (cnd, thn, els) ->
    let cndexp = evaluate cnd table in
    if cndexp = 1 then
      evaluate thn table
    else evaluate els table
  | RCmp (o, l, r) ->
    let lexp = evaluate l table in
    let rexp = evaluate r table in
    (match o with
    | "<" -> (int_of_bool (lexp < rexp))
    | "<=" -> (int_of_bool (lexp <= rexp))
    | ">" -> (int_of_bool (lexp > rexp))
    | ">=" -> (int_of_bool (lexp >= rexp))
    | "eq?" -> (int_of_bool (lexp = rexp))
    | _ -> unsupported_operator ("Unsupported compare operator: " ^ o))
  | RUnOp (o, e) ->
    let exp = evaluate e table in
    (match o with
    | "-" -> - exp
    | _ -> unsupported_operator ("Unsupported unary operator: " ^ o))
  | RBinOp (o, l, r) ->
    let lexp = evaluate l table in
    let rexp = evaluate r table in
    (match o with
    | "+" -> lexp + rexp
    | _ -> unsupported_operator ("Unsupported binary operator: " ^ o))
  | RLet (v, i, b) ->
    let iexp = evaluate i table in
    Hashtbl.add table v iexp;
    evaluate b table
  | RRead ->
    let input = read_line() in
    int_of_string input

let rec repl () =
  try
    print_string "> ";
    let program = get_input "" 0 in
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let token_list = ref tokens in
    let ast = parse_exp token_list in
    let result = evaluate ast (Hashtbl.create 5) in
    print_endline (string_of_int result);
    repl ()
  with ex ->
    print_endline "There was an error interpreting the program:";
    print_endline (Printexc.to_string ex);
    repl ()

let () = repl ()
