open RProgram
open CProgram
open List

type aregister =
  | Rsp   | Rbp
  | Rax   | Rbx
  | Rcx   | Rdx
  | Rsi   | Rdi
  | R8    | R9
  | R10   | R11
  | R12   | R13
  | R14   | R15
  | Al

type acmp =
  | AE
  | ANE
  | AL
  | ALE
  | AG
  | AGE

type aarg =
  | AVoid
  | AInt of int
  | AChar of int
  | AVar of string
  | Reg of aregister
  | Deref of aregister * int
  | DerefVar of aregister * aregister
  | ByteReg of aregister
  | GlobalValue of string
  | TypeRef of datatype

type ainstr =
  | Cqto
  | IDivq of aarg
  | IMulq of aarg * aarg
  | Addq of aarg * aarg
  | Subq of aarg * aarg
  | Movq of aarg * aarg
  | Retq
  | Negq of aarg
  | Callq of string
  | Pushq of aarg
  | Popq of aarg
  | Xorq of aarg * aarg
  | Cmpq of aarg * aarg
  | Set of acmp * aarg
  | Movzbq of aarg * aarg
  | Jmp of string
  | JmpIf of acmp * string
  | Label of string
  | AIf of (acmp * aarg * aarg) * ainstr list * aarg list list * ainstr list * aarg list list
  | AWhile of ainstr list * aarg list list * (acmp * aarg * aarg) * ainstr list * aarg list list
  | Leaq of aarg * aarg
  (* Callq (label, args, result location ) *)
  | ACallq of aarg * aarg list * aarg
  | IndirectCallq of aarg
  | AComment of string
  | XChg of aarg * aarg

type interference = ((aarg, aarg list) Hashtbl.t)
type colorgraph = ((aarg, int) Hashtbl.t)

type pdefine =
  | PDefine of string * int * aarg list * (string, datatype) Hashtbl.t * int * ainstr list
  | PDefType of string * string * string * string list * datatype

type ldefine =
  | LDefine of string * int * aarg list * (string, datatype) Hashtbl.t * int * aarg list list * ainstr list
  | LDefType of string * string * string * string list * datatype

type gdefine =
  | GDefine of string * int * aarg list * (string, datatype) Hashtbl.t * int * aarg list list * interference * ainstr list
  | GDefType of string * string * string * string list * datatype

type gcdefine =
  | GCDefine of string * int * aarg list * (string, datatype) Hashtbl.t * int * aarg list list * colorgraph * ainstr list
  | GCDefType of string * string * string * string list * datatype

type adefine =
  | ADefine of string * int * aarg list * (string, datatype) Hashtbl.t * int * int * ainstr list
  | ADefType of string * string * string * string list * datatype

type aprogram =
  AProgram of int * int * datatype * adefine list * ainstr list

type pprogram =
  PProgram of var_type * datatype * pdefine list * ainstr list

type lprogram =
  LProgram of var_type * aarg list list * datatype * ldefine list * ainstr list

type gprogram =
  GProgram of var_type * aarg list list * interference * datatype * gdefine list * ainstr list

type gcprogram =
  GCProgram of var_type * aarg list list * colorgraph * datatype * gcdefine list * ainstr list

exception AVarException of string

let get_avar_name a =
  match a with
  | AVar s -> s
  | _ -> raise (AVarException "expected variable")

let get_aarg_of_carg c : aarg =
  match c with
  | CVoid -> AVoid
  | CVar v -> AVar v
  | CInt i -> AInt i
  | CChar c -> AChar (Char.code c)
  | CBool true -> AInt 1
  | CBool false -> AInt 0
  | CGlobalValue s -> GlobalValue s
  | CFunctionRef s -> GlobalValue s

let get_acmp_of_ccmp c : acmp =
  match c with
  | CEq -> AE
  | CL -> AL
  | CLE -> ALE
  | CG -> AG
  | CGE -> AGE

let get_opposite_cmp c =
  match c with
  | AE -> ANE
  | ANE -> AE
  | AL -> AGE
  | ALE -> AG
  | AG -> ALE
  | AGE -> AL

let string_of_acmp c : string =
  match c with
  | AE -> "eq?"
  | ANE -> "neq?"
  | AL -> "<"
  | ALE -> "<="
  | AG -> ">"
  | AGE -> ">="

let string_of_register r : string =
  match r with
  | Rsp -> "rsp"
  | Rbp -> "rbp"
  | Rax -> "rax"
  | Rbx -> "rbx"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
  | Al -> "al"

let string_of_aarg a : string =
  "(" ^ (fun e ->
  match a with
  | AVoid -> "Void"
  | AInt i -> "Int " ^ (string_of_int i)
  | AChar i -> "Char " ^ Char.escaped (Char.chr i)
  | AVar s -> "Var " ^ s
  | Reg r -> "Reg " ^ (string_of_register r)
  | Deref (r, i) -> "Deref " ^ (string_of_register r) ^ " " ^ (string_of_int i)
  | DerefVar (r, off) -> "Deref " ^ (string_of_register r) ^ " " ^ (string_of_register off)
  | ByteReg r -> "ByteReg " ^ (string_of_register r)
  | GlobalValue s -> "GlobalVal " ^ s
  | TypeRef dt -> "TypeRef " ^ string_of_datatype dt
  ) a
  ^ ")"

let string_of_aarg_list a : string =
  "[" ^
  List.fold_left (fun acc e -> acc ^ string_of_aarg e ^ " ") " " a
  ^ "]"

let rec string_of_ainstrs i : string =
  (List.fold_left (fun acc s -> acc ^ string_of_ainstr s ^ "\n\t") "" i)

and string_of_ainstr a : string =
  match a with
  | Cqto -> "Cqto"
  | IDivq e -> "IDivq " ^ (string_of_aarg e)
  | IMulq (l, r) -> "IMulq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Addq (l, r) -> "Addq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Subq (l, r) -> "Subq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Movq (l, r) -> "Movq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Retq -> "Retq"
  | Negq e -> "Negq " ^ (string_of_aarg e)
  | Callq s -> "Callq " ^ s
  | Pushq e -> "Pushq " ^ (string_of_aarg e)
  | Popq e -> "Popq " ^ (string_of_aarg e)
  | Xorq (l, r) -> "Xorq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Cmpq (l, r) -> "Cmpq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Set (cmp, e) -> "Set " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg e)
  | Movzbq (l, r) -> "Movzbq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Jmp s -> "Jmp " ^ s
  | JmpIf (cmp, s) -> "JmpIf " ^ (string_of_acmp cmp) ^ " " ^ s
  | Label s -> "Label " ^ s
  | XChg (l, r) -> "XChg " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | AIf ((cmp, l, r), thn, thn_live_afters, els, els_live_afters) ->
    "If " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg l) ^ " " ^ (string_of_aarg r) ^
    "\nThnLA\t[\n\t" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" thn_live_afters) ^ "\n\t]\n" ^
    "\nThen\t[\n\t" ^ (string_of_ainstrs thn) ^ "]\n" ^
    "\nElsLA\t[\n\t" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" thn_live_afters) ^ "\n\t]\n" ^
    "Else\t[\n\t" ^ (string_of_ainstrs els) ^ "]"
  | AWhile (cnd, cnd_live_afters, (cmp, l, r), thn, thn_live_afters) ->
    "\nCndLA\t[\n\t" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" cnd_live_afters) ^ "\n\t]\n" ^
    "\t[\n\t" ^ (string_of_ainstrs cnd) ^ "]\n" ^
    "While\t" ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg l) ^ " " ^ (string_of_aarg r) ^
    "\nThnLA\t[\n\t" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" thn_live_afters) ^ "\n\t]\n" ^
    "\nThen\t[\n\t" ^ (string_of_ainstrs thn) ^ "]"
  | Leaq (s, d) -> "Leaq " ^ string_of_aarg s ^ " " ^ string_of_aarg d
  | ACallq (l, args, res) ->
    "Callq " ^ string_of_aarg l ^ ", " ^ List.fold_left (fun acc e -> acc ^ " " ^ string_of_aarg e) "" args ^ ", " ^ string_of_aarg res
  | IndirectCallq a -> "IndirectCallq " ^ string_of_aarg a
  | AComment s -> "Comment " ^ s

(* Will be obsolete when switching to Core - sexp_of *)
let rec string_of_adefs defs =
  match defs with
  | PDefine (id, _, _, _, _, instrs) :: t ->
    "\nId\t:" ^ id ^
    "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]" ^ string_of_adefs t
  | [] -> ""

let print_pprogram p =
  match p with
  | PProgram (vars, datatype, defs, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nDefs\t: \n\t[\n\t" ^ (string_of_adefs defs) ^ "]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_aprogram p =
  match p with
  | AProgram (vars_space, rootstack_space, datatype, defs, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nStack Space\t: " ^ (string_of_int vars_space) ^
      "\nRootStack Space\t: " ^ (string_of_int rootstack_space) ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_lprogram p =
  match p with
  | LProgram (vars, live_afters, datatype, defs, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nLive-Afters: [");
      List.iter (fun e -> print_endline ("\t" ^ string_of_aarg_list e)) live_afters;
      print_endline ("\t]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_interfer graph =
  print_endline "\nGraph\t: [";
  Hashtbl.iter (fun k v ->
    print_string ("\n\tNode\t: " ^ (string_of_aarg k) ^ "\n\tEdges\t: [");
    List.iter (fun e -> print_string ((string_of_aarg e) ^ ", ")) v;
    print_endline " ]";
  ) graph;
  print_endline "\t]"

let rec print_defs_graph defs =
  match defs with
  | GDefine (id, num_params, args, var_types, max_stack, lives, map, instrs) :: t ->
    print_endline ("def: " ^ id);
    print_interfer map;
    print_defs_graph t
  | [] -> ()

let print_gprogram p =
  match p with
  | GProgram (vars, live_afters, graph, datatype, defs, instrs) ->
    print_defs_graph defs;
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
    (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nLive-Afters: [\n");
      List.iter (fun e -> print_endline ("\t" ^ string_of_aarg_list e)) live_afters;
      print_endline ("]" ^
      "\nGraph\t: [");
      Hashtbl.iter (fun k v ->
        print_string ("\n\tNode\t: " ^ (string_of_aarg k) ^ "\n\tEdges\t: [");
        List.iter (fun e -> print_string ((string_of_aarg e) ^ ", ")) v;
        print_endline " ]";
      ) graph;
      print_endline ("\t]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_gcprogram p =
  match p with
  | GCProgram (vars, live_afters, graph, datatype, defs, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
    (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nLive-Afters: [\n");
      List.iter (fun e -> print_endline ("\t" ^ string_of_aarg_list e)) live_afters;
      print_endline ("]" ^
      (* "\nGraph\t: [");
      Hashtbl.iter (fun k v ->
        print_string ("\n\tNode\t: " ^ (string_of_aarg k) ^ "\n\tEdges\t: [");
        print_string (string_of_datatype v);
        print_endline " ]";
      ) graph;
      print_endline ("\t]" ^ *)
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_color_graph colors =
  print_endline "\nColor Graph:";
  Hashtbl.iter (fun k v ->
      print_endline ((string_of_aarg k) ^ " : " ^ (string_of_int v));
    ) colors;
  print_endline ""

let print_move_bias_graph tbl =
  print_endline "\nMove Bias Graph:";
  Hashtbl.iter (fun k v ->
      print_endline ((string_of_aarg k) ^ " : " ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg e)) "" v);
    )) tbl;
  print_endline ""

let os_label_prefix = "_"

exception RegisterException of string

let register_of_string s : aarg =
    match s with
    | "rsp" -> Reg Rsp
    | "rbp" -> Reg Rbp
    | "rax" -> Reg Rax
    | "rbx" -> Reg Rbx
    | "rcx" -> Reg Rcx
    | "rdx" -> Reg Rdx
    | "rsi" -> Reg Rsi
    | "rdi" -> Reg Rdi
    | "r8" -> Reg R8
    | "r9" -> Reg R9
    | "r10" -> Reg R10
    | "r11" -> Reg R11
    | "r12" -> Reg R12
    | "r13" -> Reg R13
    | "r14" -> Reg R14
    | "r15" -> Reg R15
    | "al" -> Reg Al
    | _ -> raise (RegisterException "register does not exist")

let is_var a = match a with AVar _ -> true | _ -> false
