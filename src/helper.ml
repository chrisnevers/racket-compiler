open AProgram
open RProgram

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

exception VectorLength of string

let get_vector_length vec =
  match vec with
  | TypeVector dt -> List.length dt
  | _ -> raise (VectorLength ("expected vector but received: " ^ (string_of_datatype vec)))

let is_vector vec =
  match vec with
  | TypeVector dt -> true
  | _ -> false

let split3 l =
  let rec split xs ys zs l =
    match l with
    | (x, y, z) :: t -> split (x :: xs) (y :: ys) (z :: zs) t
    | [] -> List.rev xs, List.rev ys, List.rev zs
  in
  split [] [] [] l

let get_var_id id =
  match id with
  | TypeIs (_, RVar v) -> v
  | _ -> raise (SomeError "expected RVar")

let get_avar_id id =
  match id with
  | AVar v -> v
  | _ -> raise (SomeError "expected AVar")

let tbl_to_list = fun h -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []

let sanitize_id id =
  String.map (fun c -> if c = '-' then '_' else c) id

let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let remove_duplicates xs = List.rev (List.fold_left cons_uniq [] xs)

let last l = List.hd (List.rev l)
let rm_last l = List.rev (List.tl (List.rev l))

let get_free_vars args exp =
  let rec rm_bindings tbl free typed_exp =
  match typed_exp with
  | TypeIs (dt, exp) ->
    match exp with
    | RLet (id, ie, be) ->
      Hashtbl.replace tbl id None;
      rm_bindings tbl free ie;
      rm_bindings tbl free be
    | RLambda (args, ret, e) ->
      List.iter (fun (a,_) -> Hashtbl.replace tbl a None) args;
      rm_bindings tbl free e
    | RVectorLength e | RVectorRef (e, _)
    | RPrint e | RWhile (_, e)
    | RNot e | RUnOp (_, e) ->
      rm_bindings tbl free e
    | RAnd (l, r) | ROr (l, r) | RCmp (_, l, r)
    | RBinOp (_, l, r) | RVectorSet (l, _, r) ->
      rm_bindings tbl free l;
      rm_bindings tbl free r
    | RIf (c, t, e) | RArraySet (c, t, e) ->
      rm_bindings tbl free c;
      rm_bindings tbl free t;
      rm_bindings tbl free e;
    | RBegin es | RWhen (_, es)
    | RUnless (_, es) | RVector es | RArray es ->
      List.iter (fun e -> rm_bindings tbl free e) es
    | RApply (id, es) ->
      rm_bindings tbl free id;
      List.iter (fun e -> rm_bindings tbl free e) es
    | RVar v ->
      (try let _ = Hashtbl.find tbl v in ()
      with Not_found -> Hashtbl.add free v (get_some dt))
    | RInt _ | RChar _ | RBool _ | RVoid
    | RFunctionRef _ | RCollect _
    | RAllocate _ | RGlobalValue _
    | RRead -> ()
  in
  let tbl   = Hashtbl.create 10 in
  let free  = Hashtbl.create 10 in
  List.iter (fun (a, dt) -> Hashtbl.replace tbl a None) args;
  rm_bindings tbl free exp;
  tbl_to_list free