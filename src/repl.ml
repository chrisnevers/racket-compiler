open Lexer
open Token
open Parser
open RProgram
open Uniquify
open Typecheck
open Expand

exception UnsupportedOperator of string
let unsupported_operator s = raise (UnsupportedOperator s)

exception VariableNotFound of string
let variable_not_found s = raise (VariableNotFound s)

exception IndexOutOfBounds of string
let index_out_of_bounds s = raise (IndexOutOfBounds s)

exception ReplError of string
let repl_error s = raise (ReplError s)

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

  (* Functions to get evaluate values *)
  let rec get_int_value e =
    match e with
    | TypeIs (Some TypeInt, RInt i) -> i
    | TypeIs (Some TypeInt, _)      -> get_int_value (evaluate e table)
    | TypeIs (dt, _) -> unsupported_operator ("expected int but received " ^ (string_of_datatype_option dt))
  in

  let rec get_bool_value e =
    match e with
    | TypeIs (Some TypeBool, RBool i) -> i
    | TypeIs (Some TypeBool, _)       -> get_bool_value (evaluate e table)
    | TypeIs (dt, _) -> unsupported_operator ("expected bool but received " ^ (string_of_datatype_option dt))
  in

  let rec get_vector_value e =
    match e with
    | TypeIs (_, RVector v) -> (List.map (fun i ->
        match i with
        | TypeIs (Some TypeInt, _)  -> make_tint (RInt (get_int_value i))
        | TypeIs (Some TypeBool, _) -> make_tbool (RBool (get_bool_value i))
        | TypeIs (Some TypeVoid, _) -> make_tvoid RVoid
        | TypeIs (Some vec, _)      -> TypeIs (Some vec, RVector (get_vector_value i))
        | _ -> unsupported_operator ("expected valid type"))
      v)
    | TypeIs (Some TypeVector l, _)       -> get_vector_value (evaluate e table)
    | TypeIs (dt, _) -> unsupported_operator ("expected vector but received " ^ (string_of_datatype_option dt))
  in

  let get_any_value e =
    match get_datatype_option e with
    | Some TypeInt      -> make_tint (RInt (get_int_value e))
    | Some TypeBool     -> make_tbool (RBool (get_bool_value e))
    | Some TypeVoid     -> make_tvoid RVoid
    | Some TypeVector l -> TypeIs (Some (TypeVector l), RVector (get_vector_value e))
    | None              -> unsupported_operator "unexpected datatype"
  in

  (* Main matching *)
  match ast with
  | TypeIs (dt, e) -> (
    match e with
    | RVar v -> (try Hashtbl.find table v
       with Not_found -> variable_not_found ("Variable: " ^ v ^ " used before declaration"))
    | RInt i    -> make_tint e
    | RBool b   -> make_tbool e
    | RVoid     -> make_tvoid e
    | RVector l -> TypeIs (dt, RVector (get_vector_value ast))
    | RVectorRef (v, i) ->
      let vexp = get_vector_value (evaluate v table) in List.nth vexp i
    | RVectorSet (v, i, n) ->
      (* Need to store vector for appropriate lifetime... *)
      make_tvoid (RVoid)
    | RAnd (l, r) ->
      let lexp = get_bool_value (evaluate l table) in
      let rexp = get_bool_value (evaluate r table) in
      make_tbool (RBool (if lexp then rexp else false))
    | ROr (l, r) ->
      let lexp = get_bool_value (evaluate l table) in
      let rexp = get_bool_value (evaluate r table) in
      make_tbool (RBool (if lexp then true else rexp))
    | RNot e ->
      let exp = get_bool_value (evaluate e table) in
      make_tbool (RBool (not exp))
    | RIf (cnd, thn, els) ->
      let cndexp = get_bool_value (evaluate cnd table) in
      if cndexp then evaluate thn table
      else evaluate els table
    | RCmp (o, l, r) ->
      let lexp = get_int_value (evaluate l table) in
      let rexp = get_int_value (evaluate r table) in (
      match o with
      | "<"   -> make_tbool (RBool (lexp < rexp))
      | "<="  -> make_tbool (RBool (lexp <= rexp))
      | ">"   -> make_tbool (RBool (lexp > rexp))
      | ">="  -> make_tbool (RBool (lexp >= rexp))
      | "eq?" -> make_tbool (RBool (lexp = rexp))
      | _ -> unsupported_operator ("Unsupported compare operator: " ^ o))
    | RUnOp (o, e) ->
      let exp = get_int_value (evaluate e table) in (
      match o with
      | "-" -> make_tint (RInt (- exp))
      | _ -> unsupported_operator ("Unsupported unary operator: " ^ o))
    | RBinOp (o, l, r) ->
      let lexp = get_int_value (evaluate l table) in
      let rexp = get_int_value (evaluate r table) in (
      match o with
      | "+" -> make_tint (RInt (lexp + rexp))
      | _ -> unsupported_operator ("Unsupported binary operator: " ^ o))
    | RLet (v, i, b) ->
      let iexp = evaluate i table in
      let value = get_any_value iexp in
      Hashtbl.add table v value;
      evaluate b table
    | RRead ->
      let input = read_line() in
      make_tint (RInt (int_of_string input))
    | RWhile (c, e) ->
      let return = ref TypeIs (None, RVoid);
      while get_bool_value (evaluate c table) = true do
        return := evaluate e table
      done;
      !return
    | RBegin _ -> repl_error "begin should not be in repl eval"
    | RWhen (_, _) -> repl_error "when should not be in repl eval"
    | RUnless (_, _) -> repl_error "unless should not be in repl eval"
  )

let rec repl () =
  try
    print_string "> ";
    let program = get_input "" 0 in
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let token_list = ref tokens in
    let ast = parse_typed_exp token_list in
    let expand = expand_exp_type ast in
    let typed = typecheck_exp_type expand (Hashtbl.create 5) in
    let result = evaluate typed (Hashtbl.create 5) in
    let _ = match result with
      | TypeIs (_, a) -> print_endline (string_of_rexp a)
    in
    repl ()
  with ex ->
    print_endline "There was an error interpreting the program:";
    print_endline (Printexc.to_string ex);
    repl ()

let () = repl ()
