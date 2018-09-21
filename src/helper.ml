open AProgram

let cdr = fun (_, b) -> b
let car = fun (a, _) -> a

exception SomeError of string

let get_some dt =
  match dt with
  | Some s -> s
  | None -> raise (SomeError "expected optional value to contain Some _")

let print_adjacent_aargs adjacents =
  print_endline ("[" ^ List.fold_left (fun acc e -> acc ^ string_of_aarg e ^ ",") "" adjacents ^ "]")

let print_adjacent_colors colors =
  print_endline ("[" ^ List.fold_left (fun acc e -> acc ^ string_of_int e ^ ",") "" colors ^ "]")