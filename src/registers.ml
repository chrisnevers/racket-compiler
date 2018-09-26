open AProgram

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

let root_stack_register = Reg R15
let free_ptr = "free_ptr"
let fromspace_end = "fromspace_end"
