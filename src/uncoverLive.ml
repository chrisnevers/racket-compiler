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
  | ACallq (AVar id, args, _) -> [AVar id] @ fold_left (fun acc e -> acc @ get_var_list_or_empty e) [] args
  | ACallq (_, args, _) -> fold_left (fun acc e -> acc @ get_var_list_or_empty e) [] args
  | Leaq (l, r) -> get_var_list_or_empty l
  | _ -> []

let rec uncover stmts live_after : (ainstr * aarg list) list =
  match stmts with
  | AIf ((o, l, r), thn, _, els, _) :: t ->
    let (thn_stmts, thn_live_after) = split (rev (uncover (rev thn) live_after)) in
    let (els_stmts, els_live_after) = split (rev (uncover (rev els) live_after)) in
    let live_now = sort_uniq compare (head thn_live_after @ head els_live_after @ get_var_list_or_empty l @ get_var_list_or_empty r) in
    (AIf ((o, l, r), thn_stmts, thn_live_after, els_stmts, els_live_after), live_now) :: uncover t live_now
  | AWhile (cnd, _, (o, l, r), thn, _) :: t ->
    let (cnd_stmts, cnd_live_after) = split (rev (uncover (rev cnd) (r :: live_after))) in
    let (thn_stmts, thn_live_after) = split (rev (uncover (rev thn) live_after)) in
    let live_now = sort_uniq compare (head thn_live_after @ head cnd_live_after @ get_var_list_or_empty l) in
    (AWhile (cnd_stmts, cnd_live_after, (o, l, r), thn_stmts, thn_live_after), live_now) :: uncover t live_now
  | s :: t ->
    let written = get_written_vars s in
    let read = get_read_vars s in
    let live_now = sort_uniq compare ((filter (fun e -> not (mem e written)) live_after) @ read) in
    (s, live_now) :: (uncover t live_now)
  | [] -> []

let rec uncover_defs defs =
  match defs with
  | PDefine (id, num_params, args, var_types, max_stack, instrs) :: t ->
    let result = (rev (uncover (rev instrs) [])) in
    let new_instrs, live_afters = split result in
    LDefine (id, num_params, args, var_types, max_stack, live_afters, new_instrs) :: uncover_defs t
  | PDefType (id, dt) :: t -> LDefType (id, dt) :: uncover_defs t
  | PTypeCons (id, side, dt) :: t -> LTypeCons (id, side, dt) :: uncover_defs t
  | PDefTypeNames (id, l, r) :: t -> LDefTypeNames (id, l, r) :: uncover_defs t
  | [] -> []

let uncover_live program : lprogram =
  match program with
  | PProgram (vars, datatype, defs, stmts) ->
    let new_defs = uncover_defs defs in
    let result = (rev (uncover (rev stmts) [])) in
    (* print_uncover_res result; *)
    let (new_stmts, live_afters) = split result in
    LProgram (vars, live_afters, datatype, new_defs, new_stmts)
