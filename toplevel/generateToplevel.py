modules = ['List', 'Stream']
order = ['token', 'rProgram', 'cProgram', 'aProgram', 'lexer', 'parser', 'uniquify', 'typecheck', 'flatten', 'selectInstructions', 'uncoverLive']

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
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    parse tokens\n\n
let run_uniquify program = 
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    uniquify ast\n\n
let run_typecheck program = 
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    let uniq = uniquify ast in
    typecheck uniq\n\n
let run_flatten program = 
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    let uniq = uniquify ast in
    let typed = typecheck uniq in
    flatten typed\n\n
let run_select_instrs program = 
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    let uniq = uniquify ast in
    let typed = typecheck uniq in
    let flat = flatten typed in
    select_instructions flat\n\n
let run_uncover_live program = 
    let stream = get_stream program `String in
    let tokens = scan_all_tokens stream [] in
    let ast = parse tokens in
    let uniq = uniquify ast in
    let typed = typecheck uniq in
    let flat = flatten typed in
    let instr = select_instructions flat in
    uncover_live instr
  """

top_fp.write(drivers)

top_fp.close()
