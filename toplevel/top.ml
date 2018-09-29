open List
open Stream

(* token *)

type token =
  | TProgram
  | TInt of int
  | TBool of bool
  | TVar of string
  | TArithOp of string
  | TCmpOp of string
  | TLogOp of string
  | TRead
  | TLet
  | TIf
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TPos
  | TNeg
  | TZero
  | TVector
  | TVectorSet
  | TVectorRef
  | TVoid
  | TBegin
  | TWhen
  | TUnless
  | TPrint
  | TWhile
  | TEOF

let string_of_token t =
  match t with
  | TProgram -> "Program"
  | TInt i -> "Int " ^ (string_of_int i)
  | TBool b -> "Bool " ^ (string_of_bool b)
  | TVar v -> "Var " ^ v
  | TArithOp o -> "ArithOp " ^ o
  | TCmpOp o -> "CmpOp " ^ o
  | TLogOp o -> "LogOp " ^ o
  | TRead -> "Read"
  | TLet -> "Let"
  | TIf -> "If"
  | TLParen -> "("
  | TRParen -> ")"
  | TLBracket -> "["
  | TRBracket -> "]"
  | TPos -> "pos?"
  | TNeg -> "neg?"
  | TZero -> "zero?"
  | TVector -> "vector"
  | TVectorSet -> "vector-set!"
  | TVectorRef -> "vector-ref"
  | TVoid -> "void"
  | TBegin -> "begin"
  | TWhen -> "when"
  | TUnless -> "unless"
  | TPrint -> "print"
  | TWhile -> "while"
  | TEOF -> "EOF"

let print_tokens tokens =
  List.iter (fun t -> print_string ((string_of_token t) ^ " ")) tokens;
  print_string "\n";
(* rProgram *)

type datatype =
  | TypeInt
  | TypeBool
  | TypeVoid
  | TypeVector of datatype list

type rexp_type =
  | TypeIs of datatype option * rexp

and rexp =
  | RVar of string
  | RInt of int
  | RBool of bool
  | RVoid
  | RVector of rexp_type list
  | RVectorRef of rexp_type * int
  | RVectorSet of rexp_type * int * rexp_type
  | RCollect of int
  | RAllocate of int * datatype
  | RGlobalValue of string
  | RRead
  | RAnd of rexp_type * rexp_type
  | ROr of rexp_type * rexp_type
  | RNot of rexp_type
  | RIf of rexp_type * rexp_type * rexp_type
  | RCmp of string * rexp_type * rexp_type
  | RUnOp of string * rexp_type
  | RBinOp of string * rexp_type * rexp_type
  | RLet of string * rexp_type * rexp_type
  | RBegin of rexp_type list
  | RWhen of rexp_type * rexp_type list
  | RUnless of rexp_type * rexp_type list
  | RPrint of rexp_type
  | RWhile of rexp_type * rexp_type

type rprogram =
  | RProgram of datatype option * rexp_type

let get_datatype_option et : datatype option =
  match et with
  | TypeIs (dt, _) -> dt

let rec get_datatype_options l : datatype option list =
  match l with
  | TypeIs (dt, _) :: tl -> dt :: get_datatype_options tl
  | [] -> []

exception DatatypeError of string
let datatype_error msg = raise (DatatypeError msg)

let get_datatype et : datatype =
  match et with
  | TypeIs (Some dt, _) -> dt
  | _ -> datatype_error "datatype is none"

let rec get_datatypes l : datatype list =
  match l with
  | TypeIs (Some dt, _) :: tl -> dt :: get_datatypes tl
  | [] -> []
  | _ -> datatype_error "datatype is none"

let make_tint e = TypeIs (Some TypeInt, e)
let make_tbool e = TypeIs (Some TypeBool, e)
let make_tvoid e = TypeIs (Some TypeVoid, e)
let make_tvec dt e = TypeIs (Some (TypeVector dt), e)
let make_tnone e = TypeIs (None, e)

let rec string_of_datatype dt =
  match dt with
  | TypeInt -> "int"
  | TypeBool -> "bool"
  | TypeVoid -> "void"
  | TypeVector datatypes -> "(" ^ string_of_datatypes datatypes ^ ")"

and string_of_datatypes dt =
  match dt with
  | h :: [] -> string_of_datatype h
  | h :: t -> string_of_datatype h ^ " * " ^ string_of_datatypes t
  | [] -> ""

let rec string_of_datatype_option dt : string =
  match dt with
  | Some a -> string_of_datatype a
  | None -> "None"

and string_of_datatype_options (dt: datatype option list) =
  match dt with
  | h :: [] -> string_of_datatype_option h
  | h :: t -> string_of_datatype_option h ^ " * " ^ string_of_datatype_options t
  | [] -> ""

let rec string_of_rexp e : string =
  "(" ^
  (fun e ->
  match e with
  | RVar v -> "Var " ^ v
  | RInt i -> "Int " ^ string_of_int i
  | RBool b -> if b then "#t" else "#f"
  | RVoid -> "void"
  | RAnd (l, r) -> "And " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | ROr (l, r) -> "Or " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | RNot e -> "Not " ^ (string_of_rexp_type e)
  | RIf (cnd, thn, els) -> "If " ^ (string_of_rexp_type cnd) ^ " then " ^ (string_of_rexp_type thn) ^ " else " ^ (string_of_rexp_type els)
  | RCmp (o, l, r) -> o ^ " " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | RUnOp (o, e) -> o ^ " " ^ (string_of_rexp_type e)
  | RBinOp (o, l, r) -> o ^ " " ^ (string_of_rexp_type l) ^ " " ^ (string_of_rexp_type r)
  | RLet (v, i, b) -> "Let ([Var " ^ v ^ " " ^ (string_of_rexp_type i) ^ "]) " ^ (string_of_rexp_type b)
  | RRead -> "Read"
  | RVector e -> "(" ^ string_of_rexps_type e ^ ")"
  | RVectorRef (e, i) -> "Vector-ref (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_int i) ^ ")"
  | RVectorSet (e, i, n) -> "Vector-set! (" ^ (string_of_rexp_type e) ^ ", " ^ (string_of_int i) ^ ", " ^ (string_of_rexp_type n) ^ ")"
  | RCollect i -> "Collect (" ^ string_of_int i ^ ")"
  | RAllocate (i, dt) -> "Allocate (" ^ string_of_int i ^ ", " ^ string_of_datatype dt ^ ")"
  | RGlobalValue s -> "Global-Value (" ^ s ^ ")"
  | RBegin es -> "Begin (" ^ string_of_rexps_type es ^ ")"
  | RWhen (cnd, es) -> "When (" ^ string_of_rexp_type cnd ^ ") (" ^ string_of_rexps_type es ^ ")"
  | RUnless (cnd, es) -> "Unless (" ^ string_of_rexp_type cnd ^ ") (" ^ string_of_rexps_type es ^ ")"
  | RPrint e -> "Print (" ^ string_of_rexp_type e ^ ")"
  | RWhile (cnd, e) -> "While (" ^ string_of_rexp_type cnd ^ ") (" ^ string_of_rexp_type e ^ ")"
  ) e
  ^ ")\n"

and string_of_rexps exps =
  match exps with
  | h :: [] -> string_of_rexp h
  | h :: t -> string_of_rexp h ^ ", " ^ string_of_rexps t
  | [] -> ""

and string_of_rexp_type e : string =
  match e with
  | TypeIs (dt, e) -> string_of_rexp e
  (* ^ ": " ^ (string_of_datatype_option dt) *)

and string_of_rexps_type e : string =
  match e with
  | h :: [] -> string_of_rexp_type h
  | h :: t -> string_of_rexp_type h ^ ", " ^ string_of_rexps_type t
  | [] -> ""

let print_rprogram p =
  match p with
  | RProgram (dt, e) -> print_endline ("Program :\nDatatype: " ^ (string_of_datatype_option dt) ^ "\nExp:" ^ (string_of_rexp_type e))

(* cProgram *)


type ccmp =
  | CEq | CL | CLE | CG | CGE

type carg =
  | CInt of int
  | CVar of string
  | CBool of bool
  | CVoid (* can be 1, so evaluates to true *)
  | CGlobalValue of string

type cexp =
  | CArg of carg
  | CRead
  | CPrint of datatype * carg
  | CUnOp of string * carg
  | CBinOp of string * carg * carg
  | CNot of carg
  | CCmp of ccmp * carg * carg
  | CAlloc of int * datatype
  | CVectorRef of carg * int

type cstmt =
  | CAssign of string * cexp
  | CReturn of carg
  | CIf of cexp * cstmt list * cstmt list
  | CWhile of cstmt list * cexp * cstmt list
  | CCollect of int
  | CVectorSet of carg * int * carg

type var_type = ((string, datatype) Hashtbl.t)

type cprogram =
  | CProgram of var_type * datatype * cstmt list

let string_of_ccmp o : string =
  match o with
  | CEq -> "eq?"
  | CL -> "<"
  | CLE -> "<="
  | CG -> ">"
  | CGE -> ">="

let string_of_carg a : string =
  "(" ^ (fun e ->
  match a with
  | CInt i -> "Int " ^ (string_of_int i)
  | CVar v -> "Var " ^ v
  | CBool b -> "Bool " ^ (string_of_bool b)
  | CVoid -> "Void"
  | CGlobalValue s -> "GlobalValue " ^ s
  ) a
  ^ ")"

let string_of_carg_type a : string =
  match a with
  | CInt _ -> "int"
  | CVar _ -> "var"
  | CBool _ -> "bool"
  | CVoid -> "void"
  | CGlobalValue _ -> "glbl"

let string_of_cexp e : string =
  "(" ^ (fun e ->
  match e with
  | CArg a -> "Arg " ^ (string_of_carg a)
  | CRead -> "Read"
  | CPrint (dt, a) -> "Print" ^ (string_of_carg a)
  | CUnOp (o, a) -> "UnOp " ^ o ^ " " ^ (string_of_carg a)
  | CBinOp (o, l, r) -> "BinOp " ^ o ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  | CNot a -> "Not " ^ (string_of_carg a)
  | CCmp (cmp, l, r) -> "Cmp " ^ (string_of_ccmp cmp) ^ " " ^ (string_of_carg l) ^ " " ^ (string_of_carg r)
  | CAlloc (i, dt) -> "Allocate " ^ (string_of_int i) ^ " " ^ (string_of_datatype dt)
  | CVectorRef (v, i) -> "VectorRef " ^ string_of_carg v ^ " " ^ string_of_int i
  ) e
  ^ ")"

let rec string_of_cstmts stmts : string =
  List.fold_left (fun acc s -> acc ^ string_of_cstmt s ^ "\n\t") "" stmts

and string_of_cstmt a : string =
  "(" ^ (fun e ->
  match a with
  | CAssign (v, e) -> "Assign " ^ v ^ " " ^ (string_of_cexp e)
  | CReturn a -> "Return " ^ (string_of_carg a)
  | CIf (cnd, thn, els) -> "If " ^ (string_of_cexp cnd) ^ "\n\t\t" ^ (string_of_cstmts thn) ^ "\t" ^ (string_of_cstmts els)
  | CWhile (cnds, cnda, thn) -> "While " ^ string_of_cstmts cnds ^ "\n\t\t" ^ string_of_cexp cnda ^ "\n\t\t" ^ string_of_cstmts thn
  | CCollect i -> "Collect " ^ string_of_int i
  | CVectorSet (v, i, e) -> "VectorSet " ^ string_of_carg v ^ " " ^ string_of_int i ^ " " ^ string_of_carg e
  ) a
  ^ ")"

let string_of_string_list l : string =
    List.fold_left (fun acc s -> acc ^ s ^ " ") "" l

let string_of_vars_list l : string =
  List.fold_left (fun acc s -> match s with
  | (name, dt) -> acc ^ "(" ^ name ^ ": " ^ string_of_datatype dt ^ ")") "" l

let print_cprogram program =
  match program with
  | CProgram (vars, dt, stmts) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype dt) ^
      (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nStmts\t: \n\t[\n\t" ^ (string_of_cstmts stmts) ^ "]"
    )

(* aProgram *)


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
  | AVar of string
  | Reg of aregister
  | Deref of aregister * int
  | ByteReg of aregister
  | GlobalValue of string
  | TypeRef of datatype

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
  | Leaq of aarg * aarg
  (* Callq (label, args, result location ) *)
  | ACallq of string * aarg list * aarg

type aprogram =
  AProgram of int * int * datatype * ainstr list

type pprogram =
  PProgram of var_type * datatype * ainstr list

type lprogram =
  LProgram of var_type * aarg list list * datatype * ainstr list

type interference = ((aarg, aarg list) Hashtbl.t)
type colorgraph = ((aarg, int) Hashtbl.t)

type gprogram =
  GProgram of var_type * aarg list list * interference * datatype * ainstr list

type gcprogram =
    GCProgram of var_type * aarg list list * colorgraph * datatype * ainstr list

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
  | CBool true -> AInt 1
  | CBool false -> AInt 0
  | CGlobalValue s -> GlobalValue s

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
  | AVar s -> "Var " ^ s
  | Reg r -> "Reg " ^ (string_of_register r)
  | Deref (r, i) -> "Deref " ^ (string_of_register r) ^ " " ^ (string_of_int i)
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
    "Callq " ^ l ^ ", " ^ List.fold_left (fun acc e -> acc ^ " " ^ string_of_aarg e) "" args ^ ", " ^ string_of_aarg res

let print_pprogram p =
  match p with
  | PProgram (vars, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_aprogram p =
  match p with
  | AProgram (vars_space, rootstack_space, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      "\nStack Space\t: " ^ (string_of_int vars_space) ^
      "\nRootStack Space\t: " ^ (string_of_int rootstack_space) ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]"
    )

let print_lprogram p =
  match p with
  | LProgram (vars, live_afters, datatype, instrs) ->
    print_endline (
      "Program\t: " ^ (string_of_datatype datatype) ^
      (* "\nVars\t: [" ^ (string_of_vars_list vars) ^ "]" ^ *)
      "\nLive-Afters: [");
      List.iter (fun e -> print_endline ("\t" ^ string_of_aarg_list e)) live_afters;
      print_endline ("\t]" ^
      "\nInstrs\t: \n\t[\n\t" ^ (string_of_ainstrs instrs) ^ "]")

let print_gprogram p =
  match p with
  | GProgram (vars, live_afters, graph, datatype, instrs) ->
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
  | GCProgram (vars, live_afters, graph, datatype, instrs) ->
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

(* helper *)


let cdr = fun (_, b) -> b
let car = fun (a, _) -> a

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
(* gensym *)

module Gensym =
  struct
    let cnt = ref 0

    let gen_int () =
      let c = !cnt in
      cnt := !cnt + 1;
      c

    let gen_str str =
      let c = !cnt in
      cnt := !cnt + 1;
      str ^ (string_of_int c)

    let reset () =
      cnt := 0
end

(* registers *)


(* General purpose registers: Rax, Rcx *)
let registers = [Rbx; Rdx; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14; R15]
let num_of_registers = List.length registers

let callee_save_registers = ["rbx"; "r12"; "r13"; "r14"; "r15"]
let callee_save_aregisters = [Reg Rbx; Reg R12; Reg R13; Reg R14; Reg R15]
let callee_save_stack_size = (List.length callee_save_registers) * 8

(* Don't include our general purpose registers: Rax, Rcx *)
let caller_save_registers = ["rdx"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"]
let caller_save_aregisters = [Reg Rdx; Reg Rsi; Reg Rdi; Reg R8; Reg R9; Reg R10; Reg R11]
let caller_save_stack_size = (List.length caller_save_registers) * 8

let arg_locations = [Reg Rdi; Reg Rsi; Reg Rdx; Reg Rcx; Reg R8; Reg R9]

let root_stack_register = R15
let free_ptr = "free_ptr"
let fromspace_end = "fromspace_end"

(* lexer *)


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

(* Skips white space *)
let rec next_char stream : char =
  let nc = Stream.next stream in
  match nc with
  | ' ' | '\t' | '\n' -> next_char stream
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
    | "begin"   -> TBegin
    | "when"    -> TWhen
    | "unless"  -> TUnless
    | "print"   -> TPrint
    | "while"   -> TWhile
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
    | '+' -> TArithOp "+"
    | '-' -> TArithOp "-"
    | '(' -> TLParen
    | ')' -> TRParen
    | '[' -> TLBracket
    | ']' -> TRBracket
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

(* parser *)


exception ParserError of string

let parser_error s = raise (ParserError s)

let next_token tokens = hd !tokens

let get_token tokens =
  let token = next_token tokens in
  (tokens := tl !tokens; token)

let expect_token tokens expected =
  let actual = get_token tokens in
  if actual = expected then ()
  else parser_error ("Expected " ^ (string_of_token expected) ^ " but received " ^ (string_of_token actual))

let parse_var tokens =
  let actual = get_token tokens in
  match actual with
  | TVar v -> v
  | _ -> parser_error ("Expected var but received " ^ (string_of_token actual))

let parse_int tokens : int =
  let token = get_token tokens in
  match token with
  | TInt i -> i
  | _ -> parser_error ("Expected integer but received " ^ (string_of_token token))

let rec parse_exp tokens : rexp =
  let token = get_token tokens in
  match token with
  | TLParen ->
    let exp = parse_exp tokens in
    expect_token tokens TRParen; exp
  | TVoid -> RVoid
  | TInt i -> RInt i
  | TVar v -> RVar v
  | TBool b -> RBool b
  | TBegin ->
    let exps = parse_inner_exps tokens in
    RBegin exps
    | TWhen ->
    let cnd = parse_typed_exp tokens in
    let exps = parse_inner_exps tokens in
    RWhen (cnd, exps)
  | TUnless ->
    let cnd = parse_typed_exp tokens in
    let exps = parse_inner_exps tokens in
    RUnless (cnd, exps)
  | TVector ->
    let exps = parse_inner_exps tokens in
    RVector exps
  | TVectorRef ->
    let exp = parse_typed_exp tokens in
    let index = parse_int tokens in
    RVectorRef(exp, index)
  | TVectorSet ->
    let e1 = parse_typed_exp tokens in
    let index = parse_int tokens in
    let e2 = parse_typed_exp tokens in
    RVectorSet(e1, index, e2)
  | TRead -> RRead
  | TPrint ->
    let exp = parse_typed_exp tokens in
    RPrint exp
  | TWhile ->
    let cnd = parse_typed_exp tokens in
    let exp = parse_typed_exp tokens in
    RWhile (cnd, exp)
  | TArithOp o ->
    let exp = parse_typed_exp tokens in
    (match next_token tokens with
    | TRParen -> RUnOp (o, exp)
    | _ ->
      let exp2 = parse_typed_exp tokens in
      RBinOp(o, exp, exp2))
  | TLogOp "and" ->
    let l = parse_typed_exp tokens in
    let r = parse_typed_exp tokens in
    RAnd (l, r)
  | TLogOp "or" ->
    let l = parse_typed_exp tokens in
    let r = parse_typed_exp tokens in
    ROr (l, r)
  | TLogOp "not" ->
    let exp = parse_typed_exp tokens in RNot exp
  | TPos ->
    let exp = parse_typed_exp tokens in
    RCmp (">", exp, TypeIs (None, RInt 0))
  | TNeg ->
    let exp = parse_typed_exp tokens in
    RCmp ("<", exp, TypeIs (None, RInt 0))
  | TZero ->
    let exp = parse_typed_exp tokens in
    RCmp ("eq?", exp, TypeIs (None, RInt 0))
  | TCmpOp o ->
    let l = parse_typed_exp tokens in
    let r = parse_typed_exp tokens in
    RCmp (o, l, r)
  | TLet ->
    expect_token tokens TLParen;
    expect_token tokens TLBracket;
    let v = parse_var tokens in
    let i = parse_typed_exp tokens in
    expect_token tokens TRBracket;
    expect_token tokens TRParen;
    let b = parse_typed_exp tokens in
    RLet (v, i, b)
  | TIf ->
    let cnd = parse_typed_exp tokens in
    let thn = parse_typed_exp tokens in
    let els = parse_typed_exp tokens in
    RIf (cnd, thn, els)
  | _ -> parser_error ("Did not expect token " ^ (string_of_token token))

and parse_typed_exp tokens =
  TypeIs (None, parse_exp tokens)

and parse_inner_exps tokens =
  let next = next_token tokens in
  match next with
  | TRParen -> []
  | _ ->
    let exp = parse_typed_exp tokens in
    exp :: parse_inner_exps tokens

let parse_program tokens : rprogram =
  expect_token tokens TLParen;
  expect_token tokens TProgram;
  let exp = parse_typed_exp tokens in
  expect_token tokens TRParen;
  expect_token tokens TEOF;
  RProgram (None, exp)

let parse tokens =
  let token_list = ref tokens in
  parse_program token_list

(* expand *)


let rec begin_to_let es =
  match es with
  | h :: [] -> h
  | h :: tl -> make_tnone (RLet ("_", h, begin_to_let tl))
  | [] -> make_tnone RVoid

and expand_exp exp :rexp =
  match exp with
  (* Expand sugars *)
  | RAnd (l, r) -> RIf (expand_exp_type l, expand_exp_type r, make_tbool (RBool false))
  | ROr (l, r) -> RIf (expand_exp_type l, make_tbool (RBool true), expand_exp_type r)
  | RBegin es -> expand_exp (RLet ("_", hd es, begin_to_let (tl es)))
  | RWhen (cnd, es) -> RIf (expand_exp_type cnd, expand_exp_type (make_tnone (RBegin es)), make_tnone RVoid)
  | RUnless (cnd, es) -> expand_exp (RWhen (make_tnone (RNot (cnd)), es))
  (* Expand inner expressions *)
  | RPrint e -> RPrint (expand_exp_type e)
  | RVector es -> RVector (List.map expand_exp_type es)
  | RVectorRef (e, i) -> RVectorRef (expand_exp_type e, i)
  | RVectorSet (v, i, e) -> RVectorSet (expand_exp_type v, i, expand_exp_type e)
  | RNot e -> RNot (expand_exp_type e)
  | RIf (c, t, e) -> RIf (expand_exp_type c, expand_exp_type t, expand_exp_type e)
  | RCmp (o, l, r) -> RCmp (o, expand_exp_type l, expand_exp_type r)
  | RUnOp (o, e) -> RUnOp (o, expand_exp_type e)
  | RBinOp (o, l, r) -> RBinOp (o, expand_exp_type l, expand_exp_type r)
  | RLet (v, i, b) -> RLet (v, expand_exp_type i, expand_exp_type b)
  | RWhile (c, e) -> RWhile (expand_exp_type c, expand_exp_type e)
  | _ -> exp

and expand_exp_type exp :rexp_type =
  match exp with
  | TypeIs (dt, e) -> TypeIs (dt, expand_exp e)

let expand program =
  match program with
  | RProgram (dt, e) ->
    RProgram (dt, expand_exp_type e)

(* uniquify *)


exception UniquifyError of string

let uniquify_error s = raise (UniquifyError s)

let get_var_name v table : string =
  try
    let count = Hashtbl.find table v in
    v ^ (string_of_int count)
  with Not_found -> uniquify_error ("get_var_name: Variable " ^ v ^ " is undefined")

let uniquify_name v table : string =
  let cnt = Gensym.gen_int () in
  let _ = Hashtbl.replace table v cnt in
  v ^ (string_of_int cnt)

let rec uniquify_exp ast table : rexp =
  match ast with
  | RLet (v, i, b) ->
    let iexp = uniquify_exp_type i table in
    let uniq_var = uniquify_name v table in
    let bexp = uniquify_exp_type b table in
    RLet (uniq_var, iexp, bexp)
  | RUnOp (o, e) -> RUnOp (o, uniquify_exp_type e table)
  | RBinOp (o, l, r) -> RBinOp (o, uniquify_exp_type l table, uniquify_exp_type r table)
  | RVar v -> RVar (get_var_name v table)
  | RVector es -> RVector (List.map (fun e -> uniquify_exp_type e table) es)
  | RVectorRef (e, i) -> RVectorRef (uniquify_exp_type e table, i)
  | RVectorSet (v, i, e) -> RVectorSet (uniquify_exp_type v table, i, uniquify_exp_type e table)
  | RAnd (l, r) -> RAnd (uniquify_exp_type l table, uniquify_exp_type r table)
  | ROr (l, r) -> ROr (uniquify_exp_type l table, uniquify_exp_type r table)
  | RNot e -> RNot (uniquify_exp_type e table)
  | RIf (cnd, thn, els) -> RIf (uniquify_exp_type cnd table, uniquify_exp_type thn table, uniquify_exp_type els table)
  | RCmp (o, l, r) -> RCmp (o, uniquify_exp_type l table, uniquify_exp_type r table)
  | RPrint e -> RPrint (uniquify_exp_type e table)
  | RWhile (c, e) -> RWhile (uniquify_exp_type c table, uniquify_exp_type e table)
  | _ -> ast

and uniquify_exp_type ast table : rexp_type =
  match ast with
  | TypeIs (dt, e) -> TypeIs (dt, uniquify_exp e table)

let uniquify ast : rprogram =
  match ast with
  | RProgram (_, e) -> RProgram (None, uniquify_exp_type e (Hashtbl.create 10))

(* typecheck *)


exception TypecheckError of string

let typecheck_error s = raise (TypecheckError s)

let get_var_type v table =
  try Hashtbl.find table v
  with Not_found -> typecheck_error "get_var_type: Undeclared variable"

let rec typecheck_exp exp table =
  match exp with
  | RInt i  -> make_tint (RInt i)
  | RBool b -> make_tbool (RBool b)
  | RVoid   -> make_tvoid RVoid
  | RVector exps ->
    let typed_exps = List.map (fun t -> typecheck_exp_type t table) exps in
    let datatypes = get_datatypes typed_exps in
    TypeIs (Some (TypeVector datatypes), RVector typed_exps)
  | RVectorRef (v, i) ->
    let nv = typecheck_exp_type v table in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
          let ref_type = List.nth datatypes i in
          TypeIs (Some ref_type, RVectorRef (nv, i))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-ref must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVectorSet (v, i, e) ->
    let nv = typecheck_exp_type v table in
    let dt = get_datatype nv in (
    match dt with
    | TypeVector datatypes -> (try
      let tk = List.nth datatypes i in
      let ne = typecheck_exp_type e table in
      let edt = get_datatype ne in
      if tk = edt then
        make_tvoid (RVectorSet (nv, i, ne))
      else typecheck_error ("typecheck_exp: vector-set! must operate on same type. Expected " ^ (string_of_datatype tk) ^ " but received " ^ (string_of_datatype edt))
      with Failure _ -> typecheck_error ("typecheck_exp: Cannot access " ^ (string_of_int i) ^ " field in tuple: " ^ (string_of_datatype dt)))
    | _ -> typecheck_error ("typecheck_exp: Vector-set! must operate on vector. Received: " ^ (string_of_datatype dt)))
  | RVar v -> TypeIs (get_var_type v table, RVar v)
  | RAnd (l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeBool && rdt = Some TypeBool then
      make_tbool (RAnd (nl, nr))
    else typecheck_error "typecheck_exp: And expressions must operate on boolean values"
  | ROr (l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeBool && rdt = Some TypeBool then
      make_tbool (ROr (nl, nr))
    else typecheck_error "typecheck_exp: Or expressions must operate on boolean values"
  | RNot e ->
    let ne = typecheck_exp_type e table in
    let edt = get_datatype_option ne in
    if edt = Some TypeBool then
      TypeIs (edt, RNot ne)
    else typecheck_error "typecheck_exp: Not expressions must operate on boolean values"
  | RIf (cnd, thn, els) ->
    let ncnd = typecheck_exp_type cnd table in
    let cndt = get_datatype_option ncnd in
    let nthn = typecheck_exp_type thn table in
    let thdt = get_datatype_option nthn in
    let nels = typecheck_exp_type els table in
    let eldt = get_datatype_option nels in
    if cndt <> Some TypeBool then
      typecheck_error "typecheck_exp: If condition must evaluate to boolean value"
    else if thdt = eldt then
      TypeIs (thdt, RIf (ncnd, nthn, nels))
    else typecheck_error "typecheck_exp: If condition's then and else must evaluate to same type"
  | RCmp (o, l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    (match o with
    | ">" | ">=" | "<" | "<=" ->
      if ldt = Some TypeInt && ldt = Some TypeInt then make_tbool (RCmp (o, nl, nr))
      else typecheck_error ("typecheck_exp: " ^ o ^ " operates on integers")
    | "eq?" ->
      if ldt = rdt then make_tbool (RCmp (o, nl, nr))
      else typecheck_error "typecheck_exp: eq? only compares same type"
    | _ -> typecheck_error "typecheck_exp: unexpected compare operator")
  | RUnOp (o, e) ->
    let ne = typecheck_exp_type e table in
    let edt = get_datatype_option ne in
    if edt = Some TypeInt then make_tint (RUnOp (o, ne))
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integer")
  | RBinOp (o, l, r) ->
    let nl = typecheck_exp_type l table in
    let ldt = get_datatype_option nl in
    let nr = typecheck_exp_type r table in
    let rdt = get_datatype_option nr in
    if ldt = Some TypeInt && rdt = Some TypeInt then make_tint (RBinOp (o, nl, nr))
    else typecheck_error ("typecheck_exp: " ^ o ^ " must be applied on integers")
  | RLet (v, i, b) ->
    let ni = typecheck_exp_type i table in
    let idt = get_datatype_option ni in
    let _ = Hashtbl.add table v idt in
    let nb = typecheck_exp_type b table in
    let bdt = get_datatype_option nb in
    TypeIs (bdt, RLet (v, ni, nb))
  | RRead -> make_tint (RRead)
  | RPrint e ->
    let ne = typecheck_exp_type e table in
    make_tvoid (RPrint ne)
  | RWhile (c, e) ->
    let nc = typecheck_exp_type c table in
    let ne = typecheck_exp_type e table in
    let dt = get_datatype_option ne in
    TypeIs (dt, RWhile (nc, ne))
  | RBegin _ -> typecheck_error "should not have begin in typecheck"
  | RWhen (_, _) -> typecheck_error "should not have when in typecheck"
  | RUnless (_, _) -> typecheck_error "should not have unless in typecheck"
  | RCollect _ -> typecheck_error "should not have collect in typecheck"
  | RAllocate (_, _) -> typecheck_error "should not have allocate in typecheck"
  | RGlobalValue _ -> typecheck_error "should not have globalvalue in typecheck"

and typecheck_exp_type exp table =
  match exp with
  | TypeIs (_, e) -> typecheck_exp e table

let typecheck program : rprogram =
  match program with
  | RProgram (_, e) ->
    let ne = typecheck_exp_type e (Hashtbl.create 10) in
    match ne with
    | TypeIs (dt, _) -> RProgram (dt, ne)

(* exposeAllocation *)


(*
Generates:
(let ([_ (vector-set! v 0 x0)]) ... (let ([_ (vector-set! v n-1 xn-1)])
  v) ... ))))
*)
let rec gen_vec_sets v vdt xdts xs =
  match xs with
  | [] -> TypeIs (vdt, RVar v)
  | (index, x) :: t ->
    let xdt = Some (hd xdts) in
    TypeIs (vdt, RLet (Gensym.gen_str "_",
          make_tvoid (RVectorSet (TypeIs (vdt, RVar v), index, TypeIs (xdt, RVar x))),
          gen_vec_sets v vdt (tl xdts) t))

(*
Generates:
(let ([_ (if (< (+ (global-value free_ptr) bytes)
             (global-value fromspace_end))
         (void)
         (collect bytes))])
(let ([v (allocate len type)])
*)
let gen_if_expr vecsets dt vec_name =
  let len = List.length dt in
  let bytes = 8 + (len * 8) in
  let if_expr = make_tvoid (
    RIf (make_tbool (RCmp ("<", make_tint (RBinOp ("+", make_tint (RGlobalValue free_ptr), make_tint (RInt bytes))), make_tint (RGlobalValue fromspace_end))),
    make_tvoid RVoid, make_tvoid (RCollect bytes)))
  in
  let allocate_expr = make_tvec dt (
    RLet (vec_name, make_tvec dt (RAllocate (len, TypeVector dt)), vecsets))
  in
  make_tvec dt (RLet (Gensym.gen_str "_", if_expr, allocate_expr))

(*
Generates:
(let([x0 e0])...(let([xn-1 en-1])
*)
let rec gen_exp_sets xs2es ifexp dt =
  match xs2es with
  | [] -> ifexp
  | ((i, x), e) :: t ->
    make_tvec dt (RLet (x, expose_exp_type e, gen_exp_sets t ifexp dt))

and expose_exp_type e =
  match e with
  | TypeIs (Some TypeVector dt, RVector es) ->
    let xs = List.mapi (fun index e -> (index, Gensym.gen_str "x")) es in
    let xs2es = List.combine xs es in
    let vec_name = Gensym.gen_str "v" in
    let vector_sets = gen_vec_sets vec_name (Some (TypeVector dt)) dt xs in
    let if_expr = gen_if_expr vector_sets dt vec_name in
    let exp_sets = gen_exp_sets xs2es if_expr dt in
    exp_sets
  | TypeIs (dt, e) -> TypeIs (dt, expose_exp e)

and expose_exp e =
  match e with
  | RVector v -> RVector (List.map (fun ve -> expose_exp_type ve) v)
  | RVectorRef (v, i) -> RVectorRef (expose_exp_type v, i)
  | RVectorSet (v, i, e) -> RVectorSet (expose_exp_type v, i, expose_exp_type e)
  | RAnd (l, r) -> RAnd (expose_exp_type l, expose_exp_type r)
  | ROr (l, r) -> ROr (expose_exp_type l, expose_exp_type r)
  | RNot e -> RNot (expose_exp_type e)
  | RIf (c, t, f) -> RIf (expose_exp_type c, expose_exp_type t, expose_exp_type f)
  | RCmp (o, l, r) -> RCmp (o, expose_exp_type l, expose_exp_type r)
  | RUnOp (o, e) -> RUnOp (o, expose_exp_type e)
  | RBinOp (o, l, r) -> RBinOp (o, expose_exp_type l, expose_exp_type r)
  | RLet (v, i, b) -> RLet (v, expose_exp_type i, expose_exp_type b)
  | RPrint e -> RPrint (expose_exp_type e)
  | RWhile (c, e) -> RWhile (expose_exp_type c, expose_exp_type e)
  | _ -> e

let expose_allocation program =
  match program with
  | RProgram (dt, e) -> RProgram (dt, expose_exp_type e)

(* flatten *)


exception FlattenError of string

let flatten_error s = raise (FlattenError s)

let get_var_name v name =
  match v with
  | Some name -> name
  | None -> Gensym.gen_str name

let get_carg_of_rarg a : carg =
  match a with
  | RBool b -> CBool b
  | RInt i -> CInt i
  | RVar name -> CVar name
  | RVoid -> CVoid
  | RGlobalValue label -> CGlobalValue label
  | _ -> flatten_error ("get_carg_of_rarg: Expected to receive CArg but received " ^ (string_of_rexp a))

let get_ccmp_of_rcmp o : ccmp =
  match o with
  | "eq?" -> CEq
  | "<" -> CL
  | "<=" -> CLE
  | ">" -> CG
  | ">=" -> CGE
  | _ -> flatten_error ("get_ccmp_of_rcmp: Expected compare operator but received " ^ o)

let flatten_arg ?(v=None) a : carg * cstmt list * (string * datatype) list =
  let flat_arg = get_carg_of_rarg a in
  let stmts = if v = None then [] else [CAssign (get_var_name v (string_of_carg_type flat_arg), CArg flat_arg)] in
  let var_list = [] in
  (flat_arg, stmts, var_list)

(* let rec flatten_exp ?(v=None) e : carg * cstmt list * (string * datatype) list = *)

let rec flatten_typed_exp ?(v=None) exp =
  match exp with
  | TypeIs (None, ue) -> flatten_error ("flatten: expression is untyped: " ^ (string_of_rexp ue))
  | TypeIs (Some dt, e) -> (
    match e with
    | RVar _ | RInt _ | RBool _ | RVoid | RGlobalValue _ ->
      flatten_arg e ~v:v
    | RAnd (l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "and" in
      let flat_arg = CVar var_name in
      let lif_cnd = CCmp (CEq, CBool true, larg) in
      let rif_cnd = CCmp (CEq, CBool true, rarg) in
      (* We only execute this if first condition is true, so if this condition is true, then set var to true, otherwise false *)
      let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
      (* Execute first condition, if true continue to next condition (execute its stmts then call if to see if true), otherwise short circuit to false *)
      let lif = CIf (lif_cnd, rstmts @ [rif], [CAssign (var_name, CArg (CBool false))]) in
      (* Execute lstmts see if left is true *)
      let stmts = lstmts @ [lif] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | ROr (l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "or" in
      let flat_arg = CVar var_name in
      let lif_cnd = CCmp (CEq, CBool true, larg) in
      let rif_cnd = CCmp (CEq, CBool true, rarg) in
      (* We only execute this if first condition is false, so if this condition is true, then set var to true, otherwise false *)
      let rif = CIf (rif_cnd, [CAssign (var_name, CArg (CBool true))], [CAssign (var_name, CArg (CBool false))]) in
      (* Execute first condition, if true then set var to true, else see if next condition is true *)
      let lif = CIf (lif_cnd, [CAssign (var_name, CArg (CBool true))], rstmts @ [rif]) in
      (* Execute lstmts see if left is true *)
      let stmts = lstmts @ [lif] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | RNot e ->
      let (earg, estmts, evars) = flatten_typed_exp e in
      let var_name = get_var_name v "not" in
      let flat_arg = CVar var_name in
      let stmts = estmts @ [CAssign (var_name, CNot earg)] in
      let var_list = if v = None then (var_name, dt) :: evars else evars in
      (flat_arg, stmts, var_list)
    | RIf (cnd, thn, els) ->
      let var_name = get_var_name v "if" in
      let (cnd_arg, cnd_stmts, cnd_vars) = flatten_typed_exp cnd in
      (* Assign result of then and else conditions to lhs variable / or newly created tmp *)
      let (thn_arg, thn_stmts, thn_vars) = flatten_typed_exp thn ~v:(Some var_name) in
      let (els_arg, els_stmts, els_vars) = flatten_typed_exp els ~v:(Some var_name) in
      let if_cnd = CCmp (CEq, CBool true, cnd_arg) in
      let flat_arg = CVar var_name in
      let stmts = cnd_stmts @ [CIf (if_cnd, thn_stmts, els_stmts)] in
      let var_list = if v = None then (var_name, dt) :: cnd_vars @ thn_vars @ els_vars else cnd_vars @ thn_vars @ els_vars in
      (flat_arg, stmts, var_list)
    | RWhile (cnd, thn) ->
      let var_name = get_var_name v "while" in
      let (cnd_arg, cnd_stmts, cnd_vars) = flatten_typed_exp cnd in
      let (thn_arg, thn_stmts, thn_vars) = flatten_typed_exp thn ~v:(Some var_name) in
      let while_cnd = CCmp (CEq, CBool true, cnd_arg) in
      let flat_arg = CVar var_name in
      let stmts = [CAssign (var_name, CArg (CVoid)); CWhile (cnd_stmts, while_cnd, thn_stmts)] in
      let var_list = if v = None then (var_name, dt) :: cnd_vars @ thn_vars else cnd_vars @ thn_vars in
      (flat_arg, stmts, var_list)
    | RCmp (o, l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "cmp" in
      let flat_arg = CVar var_name in
      let ccmp = get_ccmp_of_rcmp o in
      let stmts = lstmts @ rstmts @ [CAssign (var_name, CCmp (ccmp, larg, rarg))] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | RUnOp (o, e) ->
      let (earg, estmts, evars) = flatten_typed_exp e in
      let var_name = get_var_name v "unop" in
      let flat_arg = CVar var_name in
      let stmts = estmts @ [CAssign (var_name, CUnOp (o, earg))] in
      let var_list = if v = None then (var_name, dt) :: evars else evars in
      (flat_arg, stmts, var_list)
    | RBinOp (o, l, r) ->
      let (larg, lstmts, lvars) = flatten_typed_exp l in
      let (rarg, rstmts, rvars) = flatten_typed_exp r in
      let var_name = get_var_name v "binop" in
      let flat_arg = CVar var_name in
      let stmts = lstmts @ rstmts @ [CAssign (var_name, CBinOp (o, larg, rarg))] in
      let var_list = if v = None then (var_name, dt) :: lvars @ rvars else lvars @ rvars in
      (flat_arg, stmts, var_list)
    | RLet (name, i, b) ->
      (* Assign result of inner expression to the variable being declared *)
      let (iarg, istmts, ivars) = flatten_typed_exp i ~v:(Some name) in
      (* Assign result of body function to whatever variable this expression is a child of *)
      let (barg, bstmts, bvars) = flatten_typed_exp b ~v:v in
      let flat_arg = barg in
      let stmts = istmts @ bstmts in
      let var_list = (name, dt) :: ivars @ bvars in
      (flat_arg, stmts, var_list)
    | RRead ->
      let var_name = get_var_name v "read" in
      let flat_arg = CVar var_name in
      let stmts = [CAssign (var_name, CRead)] in
      let var_list = if v = None then [(var_name, dt)] else [] in
      (flat_arg, stmts, var_list)
    | RPrint e ->
      let (earg, estmts, evars) = flatten_typed_exp e in
      let edt = get_datatype e in
      let var_name = get_var_name v "print" in
      let flat_arg = CVar var_name in
      (* Revisit assign to void? *)
      let stmts = estmts @ [CAssign (var_name, CPrint (edt, earg))] in
      let var_list = if v = None then (var_name, dt) :: evars else evars in
      (flat_arg, stmts, var_list)
    | RVectorSet (vec, i, e) ->
      let (varg, vstmts, vvars) = flatten_typed_exp vec in
      let (earg, estmts, evars) = flatten_typed_exp e in
      let flat_arg = CVoid in
      let stmts = [CVectorSet (varg, i, earg)] in
      let var_list = vvars @ evars in
      (flat_arg, stmts, var_list)
    | RVectorRef (ve, i) ->
      let var_name = get_var_name v "vref" in
      let (varg, vstmts, vvars) = flatten_typed_exp ve in
      let flat_arg = CVar var_name in
      let stmts = vstmts @ [CAssign (var_name, CVectorRef (varg, i))] in
      let var_list = if v = None then (var_name, dt) :: vvars else vvars in
      (flat_arg, stmts, var_list)
    | RCollect i -> (CVoid, [CCollect i], [])
    | RAllocate (i, ty) ->
      let var_name = get_var_name v "alloc" in
      let flat_arg = CVar var_name in
      let stmts = [CAssign (var_name, CAlloc (i, ty))] in
      let var_list = if v = None then [(var_name, dt)] else [] in
      (flat_arg, stmts, var_list)
    (* Invalid expressions *)
    | RVector _ -> flatten_error "should not have vector in flatten"
    | RBegin _ -> flatten_error "should not have begin in flatten"
    | RWhen (_, _) -> flatten_error "should not have when in flatten"
    | RUnless (_, _) -> flatten_error "should not have unless in flatten"
  )

let flatten program : cprogram =
  match program with
  | RProgram (Some dt, e) ->
    let (arg, stmts, vars) = flatten_typed_exp e in
    let new_stmts = stmts @ [CReturn arg] in
    let var2dt = make_hashtable vars in
    CProgram (var2dt, dt, new_stmts)
  | _ -> flatten_error "Flatten: program does not have type"

(* selectInstructions *)


exception SelectInstructionError of string
let select_instruction_error s = raise (SelectInstructionError s)

(* Store how many times we call collect for debugging purposes *)
let collect_call_count = ref 0

let get_collect_call_count () =
  collect_call_count := !collect_call_count + 1;
  AInt !collect_call_count

let select_exp e v : ainstr list =
  match e with
  | CArg a ->
    let arg = get_aarg_of_carg a in
    [Movq (arg, v)]
  | CPrint (dt, a) ->
    let arg = get_aarg_of_carg a in
    (* Args: value, ?tag, newline *)
    let prinstrs = (match dt with
      | TypeInt -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_int"]
      | TypeBool -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_bool"]
      | TypeVoid -> [Movq (arg, Reg Rdi); Movq (AInt 1, Reg Rsi); Callq "print_void"]
      | TypeVector l -> [Movq (arg, Reg Rdi); Leaq (TypeRef dt, Reg Rsi); Movq (AInt 1, Reg Rdx); Callq "print_vector"]
    ) in
    prinstrs
    (* dont need result - Movq (Reg Rax, v) *)
  | CRead ->
    [ACallq ("read_int", [], v)]
    (* [Callq "read_int"; Movq (Reg Rax, v)] *)
  | CUnOp (o, a) ->
    let arg = get_aarg_of_carg a in
    if arg = v then [Negq v] else [Movq (arg, v); Negq v]
  | CBinOp ("+", l, r) ->
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    if larg = v then [Addq (rarg, v)] else
    if rarg = v then [Addq (larg, v)] else
    [Movq (larg, v); Addq (rarg, v)]
  | CBinOp (_, _, _) ->
    select_instruction_error "select_exp: Unsupported binary arithmetic operator"
  | CNot a ->
    let arg = get_aarg_of_carg a in
    [Movq (arg, v); Xorq (AInt 1, v)]
  | CCmp (o, l, r) ->
    let op = get_acmp_of_ccmp o in
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    (* Handle switching cmpq arg positions *)
    [Cmpq (rarg, larg); Set (op, ByteReg Al); Movzbq (ByteReg Al, v)]
  | CAlloc (i, dt) ->
    [
      Movq (GlobalValue free_ptr, v);
      Addq (AInt (8 * (i + 1)), GlobalValue free_ptr);
      Movq (v, Reg Rax);
      Leaq (TypeRef dt, Reg Rcx);
      Movq (Reg Rcx, Deref (Rax, 0))
    ]
  | CVectorRef (ve, i) ->
    let varg = get_aarg_of_carg ve in
    [Movq (varg, Reg Rax); Movq (Deref (Rax, 8 * (i + 1)), v)]

let rec select_stmts stmt : ainstr list =
  match stmt with
  | CAssign (v, e) :: t ->
    select_exp e (AVar v) @ select_stmts t
  | CReturn a :: t ->
    let arg = get_aarg_of_carg a in
    Movq (arg, Reg Rax) :: select_stmts t
  | CIf (CCmp(o, l, r), thn, els) :: t ->
    let cmp = get_acmp_of_ccmp o in
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    let thninstrs = select_stmts thn in
    let elsinstrs = select_stmts els in
    AIf ((cmp, larg, rarg), thninstrs, [], elsinstrs, []) :: select_stmts t
  | CIf (_, thn, els) :: t ->
    select_instruction_error "select_stmt: If statement must use compare to true in condition"
  | CWhile (cnd, CCmp(o, l, r), thn) :: t ->
    let cmp = get_acmp_of_ccmp o in
    let larg = get_aarg_of_carg l in
    let rarg = get_aarg_of_carg r in
    let cndinstrs = select_stmts cnd in
    let thninstrs = select_stmts thn in
    AWhile (cndinstrs, [], (cmp, larg, rarg), thninstrs, []) :: select_stmts t
  | CWhile (cnd, _, thn) :: t -> select_instruction_error "select_stmt: While statement must use compare to true in condition"
  | CCollect i :: t ->
    ACallq ("collect", [Reg root_stack_register; AInt i; get_collect_call_count ()], AVoid) :: select_stmts t
  | CVectorSet (ve, i, ne) :: t ->
    let varg = get_aarg_of_carg ve in
    let earg = get_aarg_of_carg ne in
    Movq (varg, Reg Rax) :: Movq (earg, Deref (Rax, 8 * (i + 1))) :: select_stmts t
| [] -> []

let select_instructions program : pprogram =
  match program with
  | CProgram (vars, datatype, stmts) ->
    PProgram (vars, datatype, select_stmts stmts)

(* uncoverLive *)


let get_var_list_or_empty v : aarg list =
  match v with
  | AVar _ -> [v]
  | _ -> []

let get_written_vars i =
  match i with
  | Movq (l, r) | Addq (l, r) | Subq (l, r)
  | Movzbq (l, r) | Xorq (l, r) -> get_var_list_or_empty r
  | Set (l, r) -> get_var_list_or_empty r
  | Negq e -> get_var_list_or_empty e
  | ACallq (_, _, v) -> if v != AVoid then [v] else []
  | _ -> []

let get_read_vars i =
  match i with
  | Addq (l, r) | Subq (l, r) | Cmpq (l, r) | Xorq (l, r) -> get_var_list_or_empty l @ get_var_list_or_empty r
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

(* buildInterference *)


let find_in_map key map : aarg list =
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map : unit =
  let current_value = find_in_map key map in
  Hashtbl.remove map key;
  Hashtbl.add map key (List.sort_uniq compare (value :: current_value))

let add_bidirected_edge n1 n2 map : unit =
  append_to_value n1 n2 map;
  append_to_value n2 n1 map

let rec add_bidirected_edges_if cnd (d: aarg) (targets: aarg list) map =
  match targets with
  | n :: t ->
    if cnd n then (add_bidirected_edge d n map; add_bidirected_edges_if cnd d t map)
    else add_bidirected_edges_if cnd d t map
  | [] -> ()

let rec add_directed_edges nodes targets map =
  match nodes with
  | h :: t ->
    List.iter (fun e -> append_to_value h e map) targets;
    add_directed_edges t targets map
  | [] -> ()

let get_live_vectors live_vars var_types =
  List.filter (fun v ->
    match Hashtbl.find var_types (get_avar_name v) with
    | TypeVector _ -> true
    | _ -> false
  ) live_vars

let rec build_graph stmts live_afters map var_types : interference =
  let live_vars = head (live_afters) in
  match stmts with
  | Movq (s, d) :: t | Movzbq(s, d) :: t ->
    (* add the edge (d, v) for every v of Lafter(k) unless v = d or v = s. *)
    add_bidirected_edges_if (fun v -> v <> d && v <> s) d live_vars map;
    build_graph t (tail live_afters) map var_types
  | Addq (s, d) :: t | Subq (s, d) :: t (* | XOrq :: t ? *)->
    (* add the edge (d, v) for every v of Lafter(k) unless v = d. *)
    add_bidirected_edges_if (fun v -> v <> d) d live_vars map;
    build_graph t (tail live_afters) map var_types
  | Callq _ :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_directed_edges live_vars caller_save_aregisters map;
    build_graph t (tail live_afters) map var_types
  | AIf ((c, s, d), thn_instrs, thn_lafter, els_instrs, els_lafter) :: t ->
    let _ = build_graph thn_instrs thn_lafter map var_types in
    let _ = build_graph els_instrs els_lafter map var_types in
    build_graph t (tail live_afters) map var_types
  | AWhile (cnd_instrs, cnd_lafter, (c, s, d), thn_instrs, thn_lafter) :: t ->
    let _ = build_graph cnd_instrs cnd_lafter map var_types in
    let _ = build_graph thn_instrs thn_lafter map var_types in
    build_graph t (tail live_afters) map var_types

  (* if a vector-typed variable is live during a call to the collector, it must be spilled to ensure it is visible to the collector. *)
  (* handled by adding interference edges between the call-live vector-typed variables and all the callee-save registers *)
  | ACallq ("collect", _, _) :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_directed_edges live_vars caller_save_aregisters map;
    let vec_live = get_live_vectors live_vars var_types in
    add_directed_edges vec_live callee_save_aregisters map;
    build_graph t (tail live_afters) map var_types

  (* Generic calls *)
  | ACallq (_, _, v) :: t ->
    (* add an edge (r, v) for every caller-save register r and every variable v of Lafter(k). *)
    add_directed_edges live_vars caller_save_aregisters map;
    add_bidirected_edges_if (fun e -> true) v live_vars map;
    build_graph t (tail live_afters) map var_types

  (* Otherwise *)
  | h :: t -> build_graph t (tail live_afters) map var_types
  | [] -> map

let build_interference program : gprogram =
  match program with
  | LProgram (var_types, live_afters, datatype, stmts) ->
    let map = build_graph stmts live_afters (Hashtbl.create 10) var_types in
    GProgram (var_types, live_afters, map, datatype, stmts)

(* allocateRegisters *)


exception AllocateRegistersException of string
let allocate_error msg = raise (AllocateRegistersException msg)

let find_in_map key map =
  try Hashtbl.find map key
  with Not_found -> []

let append_to_value key value map =
  let current_value = find_in_map key map in
  Hashtbl.replace map key (List.sort_uniq compare (value :: current_value))

let create_graph keys value =
  let graph = Hashtbl.create 10 in
  let rec add_to_table ks =
    match ks with
    | key :: tail -> Hashtbl.replace graph (AVar key) value; add_to_table tail
    | [] -> ()
  in
  let _ = add_to_table keys in graph

let rec get_most_saturated graph saturations =
  let current = ref (AVar "", 0) in
  Hashtbl.iter (fun k v ->
    let no_of_saturations = List.length (find_in_map k saturations) in
    if no_of_saturations >= (cdr !current) && (is_var k) then
      current := (k, no_of_saturations)) graph;
  if (car !current) == AVar "" then
    allocate_error "Could not find max saturated node"
  else (car !current)

let rec get_lowest_color adjacent_colors cur =
  match adjacent_colors with
  | h :: t ->
    if h != -1 && h = cur then
      get_lowest_color t (cur + 1)
    else
      get_lowest_color t cur
  | [] -> cur

let rec add_color_to_saturations saturations adjacents color =
  match adjacents with
  | h :: t ->
    append_to_value h color saturations;
    add_color_to_saturations saturations t color
  | [] -> ()

let get_adjacent_colors colors adjacents =
  List.sort compare (List.map (fun e -> Hashtbl.find colors e) adjacents)

let rec get_colors graph saturations colors move =
  match Hashtbl.length graph with
  | 0 -> colors
  | _ ->
    (* Pick node in graph with highest saturation *)
    let max_saturated = get_most_saturated graph saturations in
    try
    (* Find its neighboring nodes *)
    let adjacents = Hashtbl.find graph max_saturated in
    (* Find what its neighboring nodes are already assigned *)
    let adjacent_colors = get_adjacent_colors colors adjacents in
    (* Find its move bias neighboring nodes *)
    let move_adjacents = find_in_map max_saturated move in
    (* Find whats its move bias neighbors are already assigned *)
    let bias_colors = List.filter (fun e -> e != (-1) && not (List.mem e adjacent_colors))
                      (get_adjacent_colors colors move_adjacents) in
    (* Pick lowest number not in neighboring nodes *)
    let lowest_color = if bias_colors = [] then get_lowest_color adjacent_colors 0 else hd bias_colors in
    (* Add chosen color to final color map *)
    Hashtbl.replace colors max_saturated lowest_color;
    add_color_to_saturations saturations adjacents lowest_color;
    (* Remove node from processing list *)
    Hashtbl.remove graph max_saturated;
    get_colors graph saturations colors move
  with Not_found -> allocate_error ("Could not find max_saturated node in graph: " ^ (string_of_aarg max_saturated))

(* Add register colors to the saturation list of any variable live during a callq *)
let add_register_saturations sat graph =
  (* Get index of given register in the set of all assignable registers *)
  let rec get_index e l cnt =
    match l with
    | h :: t -> if h = e then cnt else get_index e t (cnt + 1)
    | [] -> -1
  in
  (* Gets the equivalent color of a register *)
  let get_register_colors k graph : int list =
    try
      let values = Hashtbl.find graph k in
      List.rev (List.fold_left (fun acc e ->
        match e with
        | Reg r ->
          let index = get_index r registers 0 in
          if index != -1 then index :: acc else acc
        | _ -> acc
      ) [] values)
    with Not_found -> []
  in
  (* Any variable that interfers with registers, add those register colors to its saturation list *)
  Hashtbl.iter (fun k v -> Hashtbl.replace sat k (get_register_colors k graph)) sat;
  sat

let print_saturation_graph saturations =
  Hashtbl.iter (fun k v ->
    print_string ((string_of_aarg k) ^ ": [");
    print_string (List.fold_left (fun acc e -> acc ^ (string_of_int e) ^ " ") "" v);
    print_endline "]";
  ) saturations

(* Removes registers (added during callq - build interference) from working set *)
let remove_registers map =
  Hashtbl.filter_map_inplace (fun k v ->
    if not (is_var k) then
    None
    else
    (* Remove anything thats not a variable from the interference graph value *)
    Some (List.filter (fun e ->
      match e with
      | AVar _ -> true
      | _ -> false
    ) v)
  ) map;
  map

let color_graph graph vars move =
  (* List of numbers a variable cannot be assigned *)
  let saturations = add_register_saturations (create_graph vars []) graph in
  (* print_saturation_graph saturations; *)
  (* The color (number) a variable is assigned *)
  let colors = create_graph vars (-1) in
  get_colors (remove_registers (Hashtbl.copy graph)) saturations colors move

let rec create_move_bias_graph instrs tbl =
  match instrs with
  | Movq (s, d) :: tl when is_var s && is_var d ->
    append_to_value s d tbl;
    append_to_value d s tbl;
    create_move_bias_graph tl tbl
  | _ :: tl -> create_move_bias_graph tl tbl
  | [] -> tbl

let allocate_registers program : gcprogram =
  match program with
  | GProgram (vars, live_afters, graph, datatype, instrs) ->
    let jvars = get_hashtable_keys vars in
    (* Create move bias graph to doc which vars are movq'd to other vars *)
    let move = create_move_bias_graph instrs (Hashtbl.create 10) in
    (* Assign each var a color unique to its adjacent nodes *)
    (* print_gprogram (GProgram (vars, live_afters, remove_registers (Hashtbl.copy graph), datatype, instrs)); *)
    let colors = color_graph graph jvars move in
    (* Reiterate over instructions & replace vars with registers *)
    (* print_gprogram (GProgram (vars, live_afters, graph, datatype, instrs)); *)
    (* print_color_graph colors; *)
    (* let new_instrs = get_new_instrs instrs colors in *)
    GCProgram (vars, live_afters, colors, datatype, instrs)

(* assignHomes *)


exception AssignHomesError of string
let assign_error msg = raise (AssignHomesError msg)

let get_register_offset arg homes stack_offset =
  try
    Hashtbl.find homes arg
  with
  | Not_found ->
    stack_offset := !stack_offset - 8;
    Hashtbl.replace homes arg !stack_offset;
    !stack_offset

(* Map the variable to a register or spill to the stack if no space *)
let get_arg_home arg homes stack_offset colors vars rootstack_offset =
  match arg with
  | AVar v ->
    let index = Hashtbl.find colors arg in
    (* If no space, spill to stack *)
    if index >= num_of_registers then
      (* If vector store to rootstack *)
      match Hashtbl.find vars (get_avar_name arg) with
      | TypeVector _ -> Deref (root_stack_register, get_register_offset arg homes rootstack_offset)
      | _ -> Deref (Rbp, get_register_offset arg homes stack_offset)
    (* If no interference, put in any reg *)
    else if index = -1 then Reg Rbx
    (* Assign to corresponding register *)
    else Reg (List.nth registers index)
  | TypeRef d -> arg
  | _ -> arg

let save_registers registers =
  let offset = ref 0 in
  let pushqs = List.map (fun r ->
    offset := !offset + 8;
    Pushq (r)
  ) registers
  in
  if !offset > 0 then pushqs @ [Subq (AInt !offset, Reg Rsp)] else []

let restore_registers registers =
  let offset = ref 0 in
  let popqs = List.map (fun r ->
    offset := !offset + 8;
    Popq (r)
  ) registers
  in
  if !offset > 0 then Addq (AInt !offset, Reg Rsp) :: popqs else []

let save_ptr_registers registers =
  let offset = ref 0 in
  let pushqs = List.map (fun r ->
    offset := !offset + 8;
    Movq (r, Deref (root_stack_register, !offset))
  ) registers
  in
  if !offset > 0 then pushqs else []

let restore_ptr_registers registers =
  let offset = ref 0 in
  let pushqs = List.map (fun r ->
    offset := !offset - 8;
    Movq (Deref (root_stack_register, !offset), r)
  ) registers
  in
  if !offset > 0 then pushqs else []

let push_call_args registers =
  if List.length registers >= List.length arg_locations then
    assign_error "too many args to function"
  else List.mapi (fun i e -> Movq (e, List.nth arg_locations i)) registers

let get_arg_homes arg homes offset colors vars rootstack_offset=
  List.map (fun e -> get_arg_home e homes offset colors vars rootstack_offset) arg

let is_atomic vars live =
  List.filter (fun v -> match Hashtbl.find vars (get_avar_name v) with | TypeVector dt -> false | _ -> true) live

let is_ptr vars live =
  List.filter (fun v -> match Hashtbl.find vars (get_avar_name v) with | TypeVector dt -> true | _ -> false) live

let rec get_instrs instrs homes offset colors live_afters vars rootstack_offset =
  match instrs with
  | [] -> []
  | Addq (a, b) :: t ->
    Addq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Subq (a, b) :: t ->
    Subq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Movq (a, b) :: t ->
    Movq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Negq a :: t ->
    Negq (get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | ACallq (l, args, v) :: t ->
    (* NEED HELP ... SOS PLZ SOME1 HELP ME *)
    let live_vars = hd live_afters in
    (* Get the variables that are atomic (: not vectors) and ptr (: vectors) *)
    let atomic_vars = is_atomic vars live_vars in
    let ptr_vars = is_ptr vars live_vars in
    (* Get the corresponding assigned homes (registers or derefs) *)
    let atomic_registers = get_arg_homes atomic_vars homes offset colors vars rootstack_offset in
    let ptr_registers = get_arg_homes ptr_vars homes offset colors vars rootstack_offset in
    (* Only save atomic registers that are caller save *)
    let save_atomic_regs = List.filter (fun e -> List.mem e caller_save_aregisters) atomic_registers in
    (* Before calling label, save the atomic and ptr registers *)
    save_registers save_atomic_regs @
    save_ptr_registers ptr_registers @
    (* Map needed args to the necessary func arg locations *)
    push_call_args (get_arg_homes args homes offset colors vars rootstack_offset) @
    (* Call the function *)
    Callq l ::
    (* Move result to variable *)
    Movq (Reg Rax, get_arg_home v homes offset colors vars rootstack_offset) ::
    (* Pop the needed live variables back off the stack *)
    restore_registers save_atomic_regs @
    restore_ptr_registers ptr_registers @
    (* Get rest of instructions *)
    get_instrs t homes offset colors (tl live_afters) vars rootstack_offset
  | Callq l :: t ->
    Callq l :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Pushq a :: t ->
    Pushq (get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Popq a :: t ->
    Popq (get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Xorq (a, b) :: t ->
    Xorq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Cmpq (a, b) :: t ->
    Cmpq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Movzbq (a, b) :: t ->
    Movzbq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Set (c, a) :: t ->
    Set (c, get_arg_home a homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Retq :: t ->
    Retq :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Jmp l :: t ->
    Jmp l:: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | JmpIf (c, l) :: t ->
    JmpIf (c, l):: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Label l :: t ->
    Label l :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | AWhile (cnd_instrs, cnd_live_afters, (c, a, b), thn_instrs, thn_live_afters) :: t ->
    let new_cnd_instrs = get_instrs cnd_instrs homes offset colors cnd_live_afters vars rootstack_offset in
    let ahome = get_arg_home a homes offset colors vars rootstack_offset in
    let bhome = get_arg_home b homes offset colors vars rootstack_offset in
    let new_thn_instrs = get_instrs thn_instrs homes offset colors thn_live_afters vars rootstack_offset in
    AWhile (new_cnd_instrs, [], (c, ahome, bhome), new_thn_instrs, []) :: (get_instrs t homes offset colors (tl live_afters) vars) rootstack_offset
    (* assign_error "while should not be in assign homes" *)
  | AIf ((c, a, b), thn_instrs, thn_live_afters, els_instrs, els_live_afters) :: t ->
    let ahome = get_arg_home a homes offset colors vars rootstack_offset in
    let bhome = get_arg_home b homes offset colors vars rootstack_offset in
    let new_thn_instrs = get_instrs thn_instrs homes offset colors thn_live_afters vars rootstack_offset in
    let new_els_instrs = get_instrs els_instrs homes offset colors els_live_afters vars rootstack_offset in
    AIf ((c, ahome, bhome), new_thn_instrs, [], new_els_instrs, []) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)
  | Leaq (a, b) :: t -> Leaq (get_arg_home a homes offset colors vars rootstack_offset, get_arg_home b homes offset colors vars rootstack_offset) :: (get_instrs t homes offset colors (tl live_afters) vars rootstack_offset)

let assign_homes program =
  match program with
  | GCProgram (vars, live_afters, colors, datatype, instrs) ->
    let homes = Hashtbl.create 10 in
    let stack_offset = ref 0 in
    let rootstack_offset = ref 0 in
    let new_instrs = get_instrs instrs homes stack_offset colors live_afters vars rootstack_offset in
    let var_space = make_multiple_of_16 (- !stack_offset) in
    let vec_space = make_multiple_of_16 (!rootstack_offset) in
    AProgram (var_space, vec_space, datatype, new_instrs)

(* lowerConditionals *)


exception LowerConditionalsException of string

let lower_conditional_error s = raise (LowerConditionalsException s)

let gen_unique label cnt =
  cnt := !cnt + 1;
  label ^ (string_of_int !cnt)

let rec lower_instructions instrs uniq_cnt =
  match instrs with
  | [] -> []
  | AIf ((c, a1, a2), thn_instrs, _, els_instrs, _) :: t ->
    let thn_label = gen_unique "thn" uniq_cnt in
    let end_label = gen_unique "end" uniq_cnt in
    Cmpq (a1, a2) :: JmpIf (c, thn_label) :: lower_instructions els_instrs uniq_cnt @
    Jmp end_label :: Label thn_label :: lower_instructions thn_instrs uniq_cnt @
    Label end_label :: lower_instructions t uniq_cnt
  | AWhile (cnd_instrs, _, (c, a1, a2), thn_instrs, _) :: t ->
    let while_label = gen_unique "while" uniq_cnt in
    let end_label = gen_unique "end" uniq_cnt in
    Label while_label :: lower_instructions cnd_instrs uniq_cnt @ Cmpq (a1, a2) :: JmpIf (get_opposite_cmp c, end_label)
    :: lower_instructions thn_instrs uniq_cnt @ Jmp while_label
    :: Label end_label :: lower_instructions t uniq_cnt
  | h :: t -> h :: lower_instructions t uniq_cnt

let lower_conditionals program =
  match program with
  | AProgram (var_space, rootstack_space, datatype, instrs) ->
    let uniq_count = ref 0 in
    let new_instrs = lower_instructions instrs uniq_count in
    AProgram (var_space, rootstack_space, datatype, new_instrs)

(* patchInstructions *)


let is_deref arg = match arg with
  | Deref _ -> true
  | _ -> false

let is_void arg = match arg with
  | AVoid -> true
  | _ -> false

let is_int arg = match arg with
  | AInt _ -> true
  | _ -> false

let rec patch_instrs instrs = match instrs with
  | [] -> []
  | Addq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Addq (Reg Rax, b) :: patch_instrs tl
    else Addq (a, b) :: patch_instrs tl
  | Subq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Subq (Reg Rax, b) :: patch_instrs tl
    else Subq (a, b) :: patch_instrs tl
  | Movq (a, b) :: tl ->
    if is_void b then patch_instrs tl else
    if is_void a then Movq (AInt 0, b) :: patch_instrs tl else
    if a = b then patch_instrs tl else
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch_instrs tl
    else Movq (a, b) :: patch_instrs tl
  | Xorq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Xorq (Reg Rax, b) :: patch_instrs tl
    else Xorq (a, b) :: patch_instrs tl
  | Movzbq (a, b) :: tl ->
    if is_deref b then
      Movzbq (a, Reg Rax) :: Movq (Reg Rax, b) :: patch_instrs tl
    else Movzbq (a, b) :: patch_instrs tl
  | Cmpq (a, b) :: tl ->
    if is_deref a && is_deref b then
      Movq (a, Reg Rax) :: Cmpq (Reg Rax, b) :: patch_instrs tl
    else if is_int a && is_int b then
      Movq (a, Reg Rax) :: Movq (b, Reg Rcx) ::Cmpq (Reg Rax, Reg Rcx) :: patch_instrs tl
    else if is_int a then
      Movq (a, Reg Rax) :: Cmpq (Reg Rax, b) :: patch_instrs tl
    else if is_int b then
      Movq (b, Reg Rax) :: Cmpq (Reg Rax, a) :: patch_instrs tl
    else Cmpq (a, b) :: patch_instrs tl
  | h :: tl -> h :: patch_instrs tl

let patch_instructions program = match program with
  | AProgram (var_space, rootstack_space, datatype, instrs) ->
    let new_instrs = patch_instrs instrs in
    AProgram (var_space, rootstack_space, datatype, new_instrs)

(* printx86 *)


exception InvalidInstructionException of string
let invalid_instruction msg = raise (InvalidInstructionException msg)

exception InvalidTypeException of string
let invalid_type msg = raise (InvalidTypeException msg)

let rec add_save_registers registers op =
  match registers with
  | reg :: t -> "\t" ^ op ^ "\t%" ^ reg ^ "\n" ^ (add_save_registers t op)
  | [] -> ""

let dt_to_x86 dt tbl =
  match dt with
  | TypeInt -> "_tint"
  | TypeBool -> "_tbool"
  | TypeVoid -> "_tvoid"
  | TypeVector l ->
    try
      Hashtbl.find tbl dt
    with
    | Not_found ->
      let label = Gensym.gen_str "_tvector" in
      let _ = Hashtbl.add tbl dt label in
      label

let arg_to_x86 arg =
  match arg with
  | GlobalValue l -> os_label_prefix ^ l ^ "(%rip)"
  | AInt i -> "$" ^ (string_of_int i)
  | Reg r | ByteReg r ->
    "%" ^ string_of_register r
  | Deref (r, i) ->
    (string_of_int i) ^ "(%" ^ string_of_register r ^ ")"
  | AVar v -> invalid_instruction ("Cannot print vars: " ^ v)
  | AVoid -> invalid_instruction ("Cannot print void")
  | TypeRef dt-> invalid_instruction ("Did not expect typeref")

let type_arg_to_x86 arg tbl =
  match arg with
  | TypeRef dt-> dt_to_x86 dt tbl ^ "(%rip)"
  | _ -> invalid_instruction ("Printx86:type_arg_to_x86: expected type arg")

let cmp_to_x86 cmp =
  match cmp with
  | AE -> "e"
  | AL -> "l"
  | ALE -> "le"
  | AG -> "g"
  | AGE -> "ge"
  | ANE -> "ne"

let rec print_instrs instrs typelbls =
  match instrs with
  | [] -> ""
  | Addq (a, b) :: tl -> "\taddq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Subq (a, b) :: tl -> "\tsubq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Movq (a, b) :: tl -> "\tmovq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Negq a :: tl -> "\tnegq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl typelbls)
  | Callq a :: tl -> "\tcallq\t" ^ os_label_prefix ^ a ^ "\n" ^ (print_instrs tl typelbls)
  | Pushq a :: tl -> "\tpushq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl typelbls)
  | Popq a :: tl -> "\tpopq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl typelbls)
  | Retq :: tl -> print_instrs tl typelbls
  | Xorq (a, b) :: tl -> "\txorq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Cmpq (a, b) :: tl -> "\tcmpq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Set (cmp, a) :: tl -> "\tset" ^ cmp_to_x86 cmp ^ "\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl typelbls)
  | Movzbq (a, b) :: tl -> "\tmovzbq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Jmp a :: tl -> "\tjmp\t\t" ^ a ^ "\n" ^ (print_instrs tl typelbls)
  | JmpIf (cmp, a) :: tl -> "\tj" ^ cmp_to_x86 cmp ^ "\t\t" ^ a ^ "\n" ^ (print_instrs tl typelbls)
  | Label l :: tl -> l ^ ":\n" ^ (print_instrs tl typelbls)
  | Leaq (a, b) :: tl -> "\tleaq\t" ^ type_arg_to_x86 a typelbls ^ ", " ^ arg_to_x86 b ^ "\n" ^ print_instrs tl typelbls
  | a :: tl -> invalid_instruction ("invalid instruction " ^ string_of_ainstr a)

let get_x86_type_variables typetbl =
  "\n\t.globl _tint\n" ^
  "_tint:\n\t.quad	0\n" ^
  "\n\t.globl _tbool\n" ^
  "_tbool:\n\t.quad	1\n" ^
  "\n\t.globl _tvoid\n" ^
  "_tvoid:\n\t.quad	2\n" ^
  "\n\t.globl _tvector\n" ^
  "_tvector:\n\t.quad	4\n\n" ^
  Hashtbl.fold (fun k v acc ->
    match k with
    | TypeVector dt ->
      (* label: *)
      acc ^ v ^ ":\n" ^
      (* type vector *)
      "\t.quad 4\n" ^
      (* length of vector/types *)
      "\t.quad " ^ string_of_int (List.length dt) ^ "\n" ^
      (* list vector datatypes *)
      List.fold_left (fun acc2 e -> acc2 ^ "\t.quad " ^ dt_to_x86 e typetbl ^ "\n") "" dt ^ "\n"
    | _ -> invalid_type "Printx86:get_x86_type_variables: expected type vector in type table"
  ) typetbl ""

let initialize rootstack heap =
  "\tmovq\t$" ^ string_of_int rootstack ^ ", %rdi\n" ^
  "\tmovq\t$" ^ string_of_int heap ^ ", %rsi\n" ^
  "\tcallq\t" ^ os_label_prefix ^ "initialize\n"

let store_rootstack_in_reg roostack =
  "\tmovq\t" ^ arg_to_x86 (GlobalValue "rootstack_begin") ^ ", " ^ arg_to_x86 (Reg root_stack_register) ^ "\n"

let zero_out_rootstack () = "\tmovq\t$0, " ^ arg_to_x86 (Reg root_stack_register) ^ "\n"

let offset_rootstack_ptr rootstack_space op =
  if rootstack_space = 0 then "" else "\t" ^ op ^ "\t$" ^ string_of_int rootstack_space ^ ", " ^ arg_to_x86 (Reg root_stack_register) ^ "\n"

let print_result datatype typetbl =
  match datatype with
  | TypeInt ->
    "\tmovq\t%rax, %rdi\n" ^
    "\tmovq\t$1, %rsi\n" ^
    "\tcallq\t" ^ os_label_prefix ^ "print_int\n"
  | TypeBool ->
    "\tmovq\t%rax, %rdi\n" ^
    "\tmovq\t$1, %rsi\n" ^
    "\tcallq\t" ^ os_label_prefix ^ "print_bool\n"
  | TypeVoid ->
    "\tmovq\t$1, %rdi\n" ^
    "\tcallq\t" ^ os_label_prefix ^ "print_void\n"
  | TypeVector l ->
    "\tleaq\t" ^ dt_to_x86 datatype typetbl ^ "(%rip), %rsi\n" ^
    "\tmovq\t$1, %rdx\n" ^
    "\tcallq\t" ^ os_label_prefix ^ "print_vector\n"

let print_x86 program =
  match program with
  | AProgram (stack_space, rootstack_space, datatype, instrs) ->
    let heap_size = 100 in
    let typetbl = Hashtbl.create 10 in
    let middle = print_instrs instrs typetbl in
    let beginning = ".data\n" ^
                    get_x86_type_variables typetbl ^
                    ".text\n" ^
                    "\t.globl " ^ os_label_prefix ^ "main\n\n" ^
                    os_label_prefix ^ "main:\n" ^
                    "\tpushq\t%rbp\n" ^
                    "\tmovq\t%rsp, %rbp\n" ^
                    (add_save_registers callee_save_registers "pushq") ^
                    "\tsubq\t$" ^ string_of_int (stack_space + callee_save_stack_size) ^ ", %rsp\n" ^
                    initialize rootstack_space heap_size ^
                    store_rootstack_in_reg root_stack_register ^
                    zero_out_rootstack () ^
                    (* Jay has it has subq? *)
                    offset_rootstack_ptr rootstack_space "addq" ^ "\n" in
    let ending =  "\n" ^ print_result datatype typetbl ^
                  offset_rootstack_ptr rootstack_space "subq" ^
                 "\taddq\t$" ^ string_of_int (stack_space + callee_save_stack_size) ^ ",\t%rsp\n" ^
                 "\tmovq\t$0,\t%rax\n" ^
                 (add_save_registers (List.rev callee_save_registers) "popq") ^
                 "\tpopq\t%rbp\n" ^
                 "\tretq\n" in
    (beginning ^ middle ^ ending)



let run_lex program =
    let stream = get_stream program `String in
    scan_all_tokens stream []


let run_parse program =
    let tokens = run_lex program in
    parse tokens


let run_expand program =
    let ast = run_parse program in
    expand ast


let run_uniquify program =
    let expand = run_expand program in
    uniquify expand


let run_typecheck program =
    let uniq = run_uniquify program in
    typecheck uniq


let run_expose program =
    let typed = run_typecheck program in
    expose_allocation typed


let run_flatten program =
    let exposed = run_expose program in
    flatten exposed


let run_select_instrs program =
    let flat = run_flatten program in
    select_instructions flat


let run_uncover_live program =
    let instr = run_select_instrs program in
    uncover_live instr


let run_build_inter program =
    let instr = run_uncover_live program in
    build_interference instr


let run_allocate_registers program =
    let instr = run_build_inter program in
    allocate_registers instr


let run_assign_homes program =
    let instr = run_allocate_registers program in
    assign_homes instr


let run_lower_conditionals program =
    let instr = run_assign_homes program in
    lower_conditionals instr


let run_patch_instructions program =
    let instr = run_lower_conditionals program in
    patch_instructions instr


let run_print_x86 program =
    let instr = run_patch_instructions program in
    print_x86 instr


