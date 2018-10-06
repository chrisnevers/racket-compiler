open AProgram

let cdr = fun (_, b) -> b
let car = fun (a, _) -> a

let heap_size = 1024

exception OutOfBoundsException of string
let out_of_bounds_error msg = raise (OutOfBoundsException msg)

exception SomeError of string

let get_some dt =
  match dt with
  | Some s -> s
  | None -> raise (SomeError "expected optional value to contain Some _")

let print_adjacent_aargs adjacents =
  print_endline ("[" ^ List.fold_left (fun acc e -> acc ^ string_of_aarg e ^ ",") "" adjacents ^ "]")

let print_adjacent_colors colors =
  print_endline ("[" ^ List.fold_left (fun acc e -> acc ^ string_of_int e ^ ",") "" colors ^ "]")

let make_hashtable assc =
  let rec add_to_table assc tbl =
    match assc with
    | (k, v) :: t -> Hashtbl.add tbl k v; add_to_table t tbl
    | _ -> SomeError "Error creating hashtable from association list"
  in
  let tbl = Hashtbl.create 10 in
  let _ = add_to_table assc tbl in tbl

let get_hashtable_keys tbl =
  let keys = ref [] in
  Hashtbl.iter (fun k v ->
    keys := k :: !keys
  ) tbl;
  !keys

let tail (l: 'a list) : 'a list = match l with
  | [] -> []
  | h :: t -> t

let head (l: 'a list) : 'a = match l with
  | [] -> []
  | h :: t -> h

let make_multiple_of_16 i =
  let remainder = i mod 16 in
  if remainder = 0 then i
  else i + (16 - remainder)

let rec print_uncover_res result =
  match result with
  | (exp, live) :: t ->
    print_endline ("exp:\t" ^ string_of_ainstr exp);
    print_endline ("live:\t" ^ (List.fold_left (fun acc e -> acc ^ string_of_aarg e ^ " ") "" live));
    print_uncover_res t
  | [] -> ()