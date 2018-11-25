modules = ['List']
order = ['token', 'rProgram', 'cProgram', 'aProgram', 'helper', 'gensym', 'registers', 'lexer', 'parser', 'expand', 'uniquify', 'typecheck', 'convertClosures', 'exposeAllocation', 'flatten', 'selectInstructions', 'uncoverLive', 'buildInterference', 'allocateRegisters', 'assignHomes', 'lowerConditionals', 'patchInstructions', 'printx86']

top_fp = open('top.ml', 'w')

for module in modules:
    top_fp.write('open ' + module + '\n')

for f in order:
    with open ('../src/' + f + '.ml') as fp:
        top_fp.write('\n(* ' + f + ' *)\n\n')
        lines = fp.readlines()
        for line in lines:
            if line.startswith('open'):
                continue
            top_fp.write(line)

drivers = """\n\n
let run_lex program =
    let stream = get_stream program `File in
    scan_all_tokens stream []\n\n
let run_parse program =
    let tokens = run_lex program in
    parse tokens\n\n
let run_expand program =
    let ast = run_parse program in
    expand ast\n\n
let run_uniquify program =
    let expand = run_expand program in
    uniquify expand\n\n
let run_typecheck program =
    let uniq = run_uniquify program in
    typecheck uniq\n\n
let run_convert program =
    let typed = run_typecheck program in
    convert_closures typed\n\n
let run_expose program =
    let reveal = run_convert program in
    expose_allocation reveal\n\n
let run_flatten program =
    let exposed = run_expose program in
    flatten exposed\n\n
let run_select_instrs program =
    let flat = run_flatten program in
    select_instructions flat\n\n
let run_uncover_live program =
    let instr = run_select_instrs program in
    uncover_live instr\n\n
let run_build_inter program =
    let instr = run_uncover_live program in
    build_interference instr\n\n
let run_allocate_registers program =
    let instr = run_build_inter program in
    allocate_registers instr\n\n
let run_assign_homes program =
    let instr = run_allocate_registers program in
    assign_homes instr\n\n
let run_lower_conditionals program =
    let instr = run_assign_homes program in
    lower_conditionals instr\n\n
let run_patch_instructions program =
    let instr = run_lower_conditionals program in
    patch_instructions instr\n\n
let run_print_x86 program =
    let instr = run_patch_instructions program in
    print_x86 instr\n\n
"""

top_fp.write(drivers)

top_fp.close()
