open AProgram
open RProgram
open Registers
open Gensym
open Helper

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
  | TypeVector l -> (
    try Hashtbl.find tbl dt
    with Not_found ->
      let label = Gensym.gen_str "_tvector" in
      let _ = Hashtbl.add tbl dt label in
      label)
  | TypeFunction (args, ret) -> (
    try Hashtbl.find tbl dt
    with Not_found ->
      let label = Gensym.gen_str "_tfunc" in
      let _ = Hashtbl.add tbl dt label in
      label)

let arg_to_x86 arg =
  match arg with
  | GlobalValue l -> os_label_prefix ^ sanitize_id l ^ "(%rip)"
  | AInt i -> "$" ^ (string_of_int i)
  | Reg r | ByteReg r ->
    "%" ^ string_of_register r
  | Deref (r, i) ->
    (string_of_int i) ^ "(%" ^ string_of_register r ^ ")"
  | AVoid -> "$1"
  | AVar v -> invalid_instruction ("Cannot print vars: " ^ v)
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
  | Cqto :: tl -> "\tcqto\n" ^ (print_instrs tl typelbls)
  | IDivq a :: tl -> "\tidivq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl typelbls)
  | IMulq (a, b) :: tl -> "\timulq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Addq (a, b) :: tl -> "\taddq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Subq (a, b) :: tl -> "\tsubq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Movq (a, b) :: tl -> "\tmovq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | Negq a :: tl -> "\tnegq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl typelbls)
  | Callq a :: tl -> "\tcallq\t" ^ os_label_prefix ^ sanitize_id a ^ "\n" ^ (print_instrs tl typelbls)
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
  | Leaq (TypeRef a, b) :: tl -> "\tleaq\t" ^ type_arg_to_x86 (TypeRef a) typelbls ^ ", " ^ arg_to_x86 b ^ "\n" ^ print_instrs tl typelbls
  | Leaq (a, b) :: tl -> "\tleaq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ print_instrs tl typelbls
  | IndirectCallq a :: tl -> "\tcallq\t*" ^ arg_to_x86 a ^ "\n" ^ print_instrs tl typelbls
  | a :: tl -> invalid_instruction ("invalid instruction " ^ string_of_ainstr a)

let get_x86_type_variables typetbl =
  "\n\t.globl _tint\n" ^
  "_tint:\n\t.quad	0\n" ^
  "\n\t.globl _tbool\n" ^
  "_tbool:\n\t.quad	1\n" ^
  "\n\t.globl _tvoid\n" ^
  "_tvoid:\n\t.quad	2\n" ^
  "\n\t.globl _tvector\n" ^
  "_tvector:\n\t.quad	4\n" ^
  "\n\t.globl _tfunc\n" ^
  "_tfunc:\n\t.quad	5\n" ^
  "\n" ^
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
    | TypeFunction (args, ret) ->
      acc ^ v ^ ":\n" ^
      "\t.quad 5\n" ^
      "\t.quad " ^ string_of_int (List.length args) ^ "\n" ^
      List.fold_left (fun acc2 e -> acc2 ^ "\t.quad " ^ dt_to_x86 e typetbl ^ "\n") "" args ^
      "\t.quad " ^ dt_to_x86 ret typetbl ^ "\n\n"
    | _ -> invalid_type "Printx86:get_x86_type_variables: expected type vector in type table"
  ) typetbl ""

let initialize rootstack heap =
  "\tmovq\t$" ^ string_of_int rootstack ^ ", %rdi\n" ^
  "\tmovq\t$" ^ string_of_int heap ^ ", %rsi\n" ^
  "\tcallq\t" ^ os_label_prefix ^ "initialize\n"

let store_rootstack_in_reg roostack =
  "\tmovq\t" ^ arg_to_x86 (GlobalValue "rootstack_begin") ^ ", " ^ arg_to_x86 (Reg root_stack_register) ^ "\n"

let zero_out_rootstack () = "\tmovq\t$0, (" ^ arg_to_x86 (Reg root_stack_register) ^ ")\n"

let offset_rootstack_ptr rootstack_space heap_size op =
  (* If no rootstack space, use 1/4 of heap_size *)
  let space = if rootstack_space = 0 then heap_size else rootstack_space in
  "\t" ^ op ^ "\t$" ^ string_of_int space ^ ", " ^ arg_to_x86 (Reg root_stack_register) ^ "\n"

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
    "\tmovq\t%rax, %rdi\n" ^
    "\tleaq\t" ^ dt_to_x86 datatype typetbl ^ "(%rip), %rsi\n" ^
    "\tmovq\t$1, %rdx\n" ^
    "\tcallq\t" ^ os_label_prefix ^ "print_vector\n"
  | TypeFunction (args, ret) ->
    "\tmovq\t%rax, %rdi\n" ^
    "\tleaq\t" ^ dt_to_x86 datatype typetbl ^ "(%rip), %rsi\n" ^
    "\tmovq\t$1, %rdx\n" ^
    "\tcallq\t" ^ os_label_prefix ^ "print_function\n"

let rec print_defs defs typetbl =
  match defs with
  | ADefine (id, num_params, args, var_types, max_stack, vec_space, instrs) :: t ->
    List.iter (fun a -> let _ = dt_to_x86 (Hashtbl.find var_types (get_avar_id a)) typetbl in ()) args;
    "\n\t.globl " ^ os_label_prefix ^ sanitize_id id ^ "\n" ^
    os_label_prefix ^ sanitize_id id ^ ":\n" ^
    "\tpushq\t%rbp\n" ^
    "\tmovq\t%rsp, %rbp\n" ^
    add_save_registers callee_save_registers "pushq" ^
    "\tsubq\t$" ^ string_of_int (max_stack + callee_save_stack_size) ^ ", %rsp\n" ^
    offset_rootstack_ptr vec_space heap_size "addq" ^ "\n" ^
    "\t# body start\n" ^
    print_instrs instrs typetbl ^
    "\t# body end\n" ^
    offset_rootstack_ptr vec_space heap_size "subq" ^ "\n" ^
    "\taddq\t$" ^ string_of_int (max_stack + callee_save_stack_size) ^ ", %rsp\n" ^
    add_save_registers (List.rev callee_save_registers) "popq" ^
    "\tpop\t\t%rbp\n" ^
    "\tretq\n\n" ^
    print_defs t typetbl
  | [] -> ""

let print_x86 program =
  match program with
  | AProgram (stack_space, rootstack_space, datatype, defs, instrs) ->
    let typetbl = Hashtbl.create 10 in
    let middle = print_instrs instrs typetbl in
    let defines = print_defs defs typetbl in
    let beginning = ".data\n" ^
                    get_x86_type_variables typetbl ^
                    ".text\n" ^
                    defines ^
                    "\t.globl " ^ os_label_prefix ^ "main\n\n" ^
                    os_label_prefix ^ "main:\n" ^
                    "\tpushq\t%rbp\n" ^
                    "\tmovq\t%rsp, %rbp\n" ^
                    (add_save_registers callee_save_registers "pushq") ^
                    "\tsubq\t$" ^ string_of_int (stack_space + callee_save_stack_size) ^ ", %rsp\n" ^
                    initialize rootstack_space heap_size ^
                    store_rootstack_in_reg root_stack_register ^
                    zero_out_rootstack () ^
                    offset_rootstack_ptr rootstack_space heap_size "addq" ^ "\n" in
    let ending =  "\n" ^ print_result datatype typetbl ^
                  offset_rootstack_ptr rootstack_space heap_size "subq" ^
                 "\taddq\t$" ^ string_of_int (stack_space + callee_save_stack_size) ^ ",\t%rsp\n" ^
                 "\tmovq\t$0,\t%rax\n" ^
                 (add_save_registers (List.rev callee_save_registers) "popq") ^
                 "\tpopq\t%rbp\n" ^
                 "\tretq\n" in
    (beginning ^ middle ^ ending)
