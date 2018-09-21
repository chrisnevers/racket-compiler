open AProgram

exception InvalidInstructionException of string

let invalid_instruction msg = raise (InvalidInstructionException msg)

let rec add_save_registers registers op =
  match registers with
  | reg :: t -> "\t" ^ op ^ "\t%" ^ reg ^ "\n" ^ (add_save_registers t op)
  | [] -> ""

let arg_to_x86 arg =
  match arg with
  | AInt i -> "$" ^ (string_of_int i)
  | Reg r | ByteReg r ->
    "%" ^ string_of_register r
  | Deref (r, i) ->
    (string_of_int i) ^ "(%" ^ string_of_register r ^ ")"
  | AVar v -> invalid_instruction ("Cannot print vars: " ^ v)
  | AVoid -> invalid_instruction ("Cannot print void")

let cmp_to_x86 cmp =
  match cmp with
  | AE -> "e"
  | AL -> "l"
  | ALE -> "le"
  | AG -> "g"
  | AGE -> "ge"

let rec print_instrs instrs =
  match instrs with
  | [] -> ""
  | Addq (a, b) :: tl -> "\taddq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Subq (a, b) :: tl -> "\tsubq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Movq (a, b) :: tl -> "\tmovq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Negq a :: tl -> "\tnegq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Callq a :: tl -> "\tcallq\t" ^ os_label_prefix ^ a ^ "\n" ^ (print_instrs tl)
  | Pushq a :: tl -> "\tpushq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Popq a :: tl -> "\tpopq\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Retq :: tl -> print_instrs tl
  | Xorq (a, b) :: tl -> "\txorq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Cmpq (a, b) :: tl -> "\tcmpq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Set (cmp, a) :: tl -> "\tset" ^ cmp_to_x86 cmp ^ "\t" ^ arg_to_x86 a ^ "\n" ^ (print_instrs tl)
  | Movzbq (a, b) :: tl -> "\tmovzbq\t" ^ arg_to_x86 a ^ ", " ^ arg_to_x86 b ^ "\n" ^ (print_instrs tl)
  | Jmp a :: tl -> "\tjmp\t\t" ^ a ^ "\n" ^ (print_instrs tl)
  | JmpIf (cmp, a) :: tl -> "\tj" ^ cmp_to_x86 cmp ^ "\t\t" ^ a ^ "\n" ^ (print_instrs tl)
  | Label l :: tl -> l ^ ":\n" ^ (print_instrs tl)
  | _ -> invalid_instruction "invalid instruction"

let print_x86 program =
  match program with
  | AProgram (space, datatype, instrs) ->
    let beginning = "\t.globl " ^ os_label_prefix ^ "main\n" ^
                    os_label_prefix ^ "main:\n" ^
                    "\tpushq\t%rbp\n" ^
                    "\tmovq\t%rsp, %rbp\n" ^
                    (add_save_registers callee_save_registers "pushq") ^
                    "\tsubq\t$" ^ (string_of_int (space + callee_save_stack_size)) ^ ", %rsp\n\n" in
    let middle = print_instrs instrs in
    let ending = "\n\tmovq\t%rax, %rdi\n" ^
                 "\tcallq\t" ^ os_label_prefix ^ (
                   match datatype with
                   | TypeInt -> "print_int\n"
                   | TypeBool -> "print_bool\n"
                   | TypeVoid -> "print_unit\n"
                   | TypeVector l -> "print_vector\n"
                  ) ^
                 "\taddq\t$" ^ (string_of_int (space + callee_save_stack_size)) ^ ",\t%rsp\n" ^
                 "\tmovq\t$0,\t%rax\n" ^
                 (add_save_registers (List.rev callee_save_registers) "popq") ^
                 "\tpopq\t%rbp\n" ^
                 "\tretq\n" in
    (beginning ^ middle ^ ending)
