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
  | AL
  | ALE
  | AG
  | AGE

type aarg =
  | AVoid
  | AInt of int
  | AVar of string
  | Reg of aregister
  | Deref of aregister * int
  | ByteReg of aregister

type ainstr =
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

type aprogram =
  AProgram of int * datatype * ainstr list

type pprogram =
  PProgram of string list * datatype * ainstr list

type lprogram =
  LProgram of string list * aarg list list * datatype * ainstr list

type interference = ((aarg, aarg list) Hashtbl.t)

type gprogram =
  GProgram of string list * interference * datatype * ainstr list

let get_aarg_of_carg c : aarg =
  match c with
  | CVoid -> AVoid
  | CVar v -> AVar v
  | CInt i -> AInt i
  | CBool true -> AInt 1
  | CBool false -> AInt 0

let get_acmp_of_ccmp c : acmp =
  match c with
  | CEq -> AE
  | CL -> AL
  | CLE -> ALE
  | CG -> AG
  | CGE -> AGE

let string_of_acmp c : string =
  match c with
  | AE -> "eq?"
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
  | AVar s -> "Var " ^ s
  | Reg r -> "Reg " ^ (string_of_register r)
  | Deref (r, i) -> "Deref " ^ (string_of_register r) ^ " " ^ (string_of_int i)
  | ByteReg r -> "ByteReg " ^ (string_of_register r)
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
  | Set (cmp, e) -> "JmpIf " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg e)
  | Movzbq (l, r) -> "Movzbq " ^ (string_of_aarg l) ^ " " ^  (string_of_aarg r)
  | Jmp s -> "Jmp " ^ s
  | JmpIf (cmp, s) -> "JmpIf " ^ (string_of_acmp cmp) ^ " " ^ s
  | Label s -> "Label " ^ s
  | AIf ((cmp, l, r), thn, thn_live_afters, els, els_live_afters) ->
    "If " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg l) ^ " " ^ (string_of_aarg r) ^
    "\n\t[\n\t" ^ (string_of_ainstrs thn) ^ "]\nThen Live:\t[" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" thn_live_afters) ^ "]\n[\n\t"
    ^ (string_of_ainstrs els) ^ "]\nElse Live:\t[" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" els_live_afters) ^ "]"
  | AWhile (cnd, cnd_live_afters, (cmp, l, r), thn, thn_live_afters) ->
    (string_of_ainstrs cnd) ^ "]\nThen Live:\t[" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" cnd_live_afters) ^ "]\n" ^ "\n\t[\n\t" ^
    "While " ^ (string_of_acmp cmp) ^ " " ^ (string_of_aarg l) ^ " " ^ (string_of_aarg r) ^
    "\n\t[\n\t" ^ (string_of_ainstrs thn) ^ "]\nThen Live:\t[" ^ (List.fold_left (fun acc e -> acc ^ (string_of_aarg_list e)) "" thn_live_afters) ^ "]\n"

let print_pprogram p =
  match p with
  | PProgram (vars, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_aprogram p =
  match p with
  | AProgram (vars_space, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nSpace\t: " ^ (string_of_int vars_space) ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_lprogram p =
  match p with
  | LProgram (vars, live_afters, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nLive-Afters: [");
      List.iter (fun e -> print_endline ("\t" ^ string_of_aarg_list e)) live_afters;
      print_endline ("\t]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_gprogram p =
  match p with
  | GProgram (vars, graph, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nVars\t: [" ^ (string_of_string_list vars) ^ "]" ^
      "\nGraph\t: [");
      Hashtbl.iter (fun k v ->
        print_string ("\n\tNode\t: " ^ (string_of_aarg k) ^ "\n\tEdges\t: [");
        List.iter (fun e -> print_string ((string_of_aarg e) ^ ", ")) v;
        print_endline " ]";
      ) graph;
      print_endline ("\t]" ^
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

let callee_save_registers = ["rbx"; "r12"; "r13"; "r14"; "r15"]
let caller_save_registers = ["rax"; "rdx"; "rcx"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"]
let callee_save_aregisters = [Reg Rbx; Reg R12; Reg R13; Reg R14; Reg R15]
let caller_save_aregisters = [Reg Rax; Reg Rdx; Reg Rcx; Reg Rsi; Reg Rdi; Reg R8; Reg R9; Reg R10; Reg R11]
let os_label_prefix = "_"
let callee_save_stack_size = (List.length callee_save_registers) * 8

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
