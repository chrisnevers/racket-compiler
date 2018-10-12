open AProgram
open CProgram
open Helper
open List

let get_var_list_or_empty v : aarg list =
  match v with
  | AVar _ -> [v]
  | _ -> []

let get_written_vars i =
  match i with
  | Cqto | IDivq _ -> [Reg Rdx]
  | Movq (l, r) | Addq (l, r) | Subq (l, r)
  | Movzbq (l, r) | Xorq (l, r) -> get_var_list_or_empty r
  | IMulq (l, r) -> Reg Rdx :: get_var_list_or_empty r
  | Set (l, r) -> get_var_list_or_empty r
  | Negq e -> get_var_list_or_empty e
  | ACallq (_, _, v) -> if v != AVoid then [v] else []
  | _ -> []

let get_read_vars i =
  match i with
  | IDivq e -> Reg Rdx :: get_var_list_or_empty e
  | IMulq (l, r) | Addq (l, r) | Subq (l, r) | Cmpq (l, r) | Xorq (l, r) -> get_var_list_or_empty l @ get_var_list_or_empty r
  | Movq (l, r) | Movzbq (l, r) -> get_var_list_or_empty l
  | Negq e -> get_var_list_or_empty e
  | ACallq (_, args, _) -> List.fold_left (fun acc e -> acc @ get_var_list_or_empty e) [] args
  | _ -> []

let rec uncover stmts live_after : (ainstr * aarg list) list =
  match stmts with
  | AIf ((o, l, r), thn, _, els, _) :: t ->
    let (thn_stmts, thn_live_after) = List.split (List.rev (uncover (List.rev thn) live_after)) in
    let (els_stmts, els_live_after) = List.split (List.rev (uncover (List.rev els) live_after)) in
    let live_now = List.sort_uniq compare (head thn_live_after @ head els_live_after @ get_var_list_or_empty l @ get_var_list_or_empty r) in
    (AIf ((o, l, r), thn_stmts, thn_live_after, els_stmts, els_live_after), live_now) :: uncover t live_now
  | AWhile (cnd, _, (o, l, r), thn, _) :: t ->
    let (cnd_stmts, cnd_live_after) = List.split (List.rev (uncover (List.rev cnd) (r :: live_after))) in
    let (thn_stmts, thn_live_after) = List.split (List.rev (uncover (List.rev thn) live_after)) in
    let live_now = List.sort_uniq compare (head thn_live_after @ head cnd_live_after @ get_var_list_or_empty l) in
    (AWhile (cnd_stmts, cnd_live_after, (o, l, r), thn_stmts, thn_live_after), live_now) :: uncover t live_now
  | s :: t ->
    let written = get_written_vars s in
    let read = get_read_vars s in
    let live_now = List.sort_uniq compare ((List.filter (fun e -> not (List.mem e written)) live_after) @ read) in
    (s, live_now) :: (uncover t live_now)
  | [] -> []

let uncover_live program : lprogram =
  match program with
  | PProgram (vars, datatype, stmts) ->
    let result = (List.rev (uncover (List.rev stmts) [])) in
    (* print_uncover_res result; *)
    let (new_stmts, live_afters) = List.split result in
    LProgram (vars, live_afters, datatype, new_stmts)
