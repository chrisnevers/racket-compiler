modules = ['List', 'Stream']
order = ['token', 'rProgram', 'cProgram', 'aProgram', 'lexer', 'parser', 'uniquify', 'typecheck', 'flatten', 'selectInstructions', 'uncoverLive', 'buildInterference']

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
    let stream = get_stream program `String in
    scan_all_tokens stream []\n\n
let run_parse program = 
    let tokens = run_lex program in
    parse tokens\n\n
let run_uniquify program = 
    let ast = run_parse program in
    uniquify ast\n\n
let run_typecheck program = 
    let uniq = run_uniquify program in
    typecheck uniq\n\n
let run_flatten program = 
    let typed = run_typecheck program in
    flatten typed\n\n
let run_select_instrs program = 
    let flat = run_flatten program in
    select_instructions flat\n\n
let run_uncover_live program = 
    let instr = run_select_instrs program in
    uncover_live instr\n\n
let run_build_inter program = 
    let instr = run_uncover_live program in
    build_interference instr\n\n
"""

top_fp.write(drivers)

top_fp.close()
