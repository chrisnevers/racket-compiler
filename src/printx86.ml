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

let rec dt_to_x86 dt tbl =
  match dt with
  | TypeInt -> "_tint"
  | TypeChar -> "_tchar"
  | TypeBool -> "_tbool"
  | TypeVoid -> "_tvoid"
  | TypeArray l -> begin try Hashtbl.find tbl dt
    with Not_found ->
      let label = Gensym.gen_str "_tarray" in
      let _ = Hashtbl.add tbl dt label in
      (* dt_to_x86 l tbl; *)
      label
    end
  | TypeVector l -> begin try Hashtbl.find tbl dt
    with Not_found ->
      let label = Gensym.gen_str "_tvector" in
      let _ = Hashtbl.add tbl dt label in
      List.iter (fun a -> let _ = dt_to_x86 a tbl in ()) l;
      label
    end
  | TypeFunction (args, ret) -> begin try Hashtbl.find tbl dt
    with Not_found ->
      let label = Gensym.gen_str "_tfunc" in
      let _ = Hashtbl.add tbl dt label in
      List.iter (fun a -> let _ = dt_to_x86 a tbl in ()) args;
      let _ = dt_to_x86 ret tbl in
      label
    end
  | TypePlus (l, r) -> begin try Hashtbl.find tbl dt
    with Not_found ->
      let label = Gensym.gen_str "_tplus" in
      let _ = Hashtbl.add tbl dt label in
      label
    end
  | TypeFix dt -> dt_to_x86 dt tbl
  | TypeForAll (s, idt) -> begin try Hashtbl.find tbl dt
    with Not_found ->
      let label = "_forAll" ^ s in
      dt_to_x86 idt tbl;
      let _ = Hashtbl.add tbl dt label in
      label
    end
  | TypeUser s | TypeVar s ->  begin try Hashtbl.find tbl dt
    with Not_found ->
      let label = "_" ^ s in
      let _ = Hashtbl.add tbl dt label in
      label
    end

let arg_to_x86 arg =
  match arg with
  | GlobalValue l -> os_label_prefix ^ sanitize_id l ^ "(%rip)"
  | AInt i | AChar i -> "$" ^ (string_of_int i)
  | Reg r | ByteReg r ->
    "%" ^ string_of_register r
  | Deref (r, i) ->
    (string_of_int i) ^ "(%" ^ string_of_register r ^ ")"
  | DerefVar (r, off) ->
    "(%" ^ (string_of_register r) ^ ", %" ^ string_of_register off ^ ")"
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
  | AComment s :: tl -> "\t# " ^ s ^ "\n" ^ print_instrs tl typelbls
  | XChg (a, b) :: tl -> "\txchg\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl typelbls)
  | a :: tl -> invalid_instruction ("invalid instruction " ^ string_of_ainstr a)

(* let type_tag dt =
  match dt with
  | TypeInt -> 0
  | TypeBool -> 1
  | TypeVoid -> 2
  | TypeVector _ -> 4
  | TypeFunction _ -> 5
  | TypeArray _ -> 6
  | TypeChar -> 7
  | TypePlus _ -> 8
  | TypeUser _ -> 9
  | TypeForAll _ -> 10
  | TypeVar _ -> 11 *)

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
  "\n\t.globl _tarray\n" ^
  "_tarray:\n\t.quad 6\n" ^
  "\n\t.globl _tchar\n" ^
  "_tchar:\n\t.quad 7\n" ^
  "\n\t.globl _tplus\n" ^
  "_tplus:\n\t.quad 8\n" ^
  "\n\t.globl _tuser\n" ^
  "_tuser:\n\t.quad 9\n" ^
  "\n\t.globl _tforall\n" ^
  "_tforall:\n\t.quad 10\n" ^
  "\n\t.globl _tvar\n" ^
  "_tvar:\n\t.quad 11\n" ^
  "\n" ^
  Hashtbl.fold (fun k v acc ->
    match k with
    | TypeArray dt ->
      acc ^ v ^ ":\n" ^
      (* type array *)
      "\t.quad 6\n" ^
      "\t.quad " ^ dt_to_x86 dt typetbl ^ "\n\n"
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
    | TypePlus (l, r) -> acc (* this was printed earlier *)
    | TypeFix _ -> acc (* this was printed earlier *)
    | TypeVar id ->
      acc ^ v ^ ":\n" ^
      "\t.quad 11\n" ^
      "\t.quad _" ^ id ^ "_str \n\n"
    | TypeForAll (s, dt) ->
      acc ^
      "_" ^ s ^ "_str:\n\t.string \"" ^ s ^ "\"\n\n" ^
      v ^ ":\n" ^
      "\t.quad 10\n" ^
      (* "\t.quad " ^ string_of_int (type_tag dt) ^ "\n" ^ *)
      "\t.quad _" ^ s ^ "_str \n" ^
      "\t.quad " ^ dt_to_x86 dt typetbl ^ "\n\n"
    | _ -> invalid_type ("Printx86: get_x86_type_variables: " ^ string_of_datatype k)
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
  | d :: t -> print_defs t typetbl
  | [] -> ""

let rec get_type_cons defs typetbl deftbl =
  match defs with
  | [] -> ""
  | ADefTypeNames (id, l, r) :: t ->
    Hashtbl.add deftbl id (TypeVar l, TypeVar r);
    get_type_cons t typetbl deftbl
  | ATypeCons (id, s, TypeFix (TypeForAll (_, TypePlus (l, r)))) :: t ->
    "_" ^ id ^ "_str:\n\t.string \"" ^ id ^ "\"\n\n" ^
    "_" ^ id ^ ":\n" ^
    "\t.quad 9\n" ^
    "\t.quad _" ^ id ^ "_str \n" ^
    "\t.quad " ^ dt_to_x86 (if s = Left then l else r) typetbl ^ "\n\n"
    ^ get_type_cons t typetbl deftbl
  | ADefType (id, TypeFix (TypeForAll (_, TypePlus (l, r)))) :: t ->
    let label = "_" ^ id in
    let (l_id, r_id) = Hashtbl.find deftbl id in
    let _ = Hashtbl.add typetbl (TypePlus (l, r)) label in
    label ^ "_str:\n\t.string \"" ^ id ^ "\"\n\n" ^
    label ^ ":\n" ^
    "\t.quad 8\n" ^
    "\t.quad _" ^ id ^ "_str \n" ^
    "\t.quad " ^ dt_to_x86 l_id typetbl ^ "\n" ^
    "\t.quad " ^ dt_to_x86 r_id typetbl ^ "\n\n"
    ^ get_type_cons t typetbl deftbl
  | _ :: t -> get_type_cons t typetbl deftbl

let print_x86 program =
  match program with
  | AProgram (stack_space, rootstack_space, datatype, defs, instrs) ->
    let typetbl = Hashtbl.create 10 in
    let type_cons = get_type_cons defs typetbl (Hashtbl.create 10) in
    let middle = print_instrs instrs typetbl in
    let defines = print_defs defs typetbl in
    let beginning = ".data\n" ^
                    get_x86_type_variables typetbl ^
                    type_cons ^
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
    let ending =  "\n" ^ offset_rootstack_ptr rootstack_space heap_size "subq" ^
                 "\taddq\t$" ^ string_of_int (stack_space + callee_save_stack_size) ^ ",\t%rsp\n" ^
                 "\tmovq\t$0,\t%rax\n" ^
                 (add_save_registers (List.rev callee_save_registers) "popq") ^
                 "\tpopq\t%rbp\n" ^
                 "\tretq\n" in
    (beginning ^ middle ^ ending)
