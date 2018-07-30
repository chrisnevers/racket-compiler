modules = ['List', 'Stream']
order = ['token', 'rProgram', 'cProgram', 'lexer', 'parser', 'uniquify', 'typecheck', 'flatten']

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

drivers = 
"""\n\nlet run_parse program = 
  let stream = get_stream program `String in
  let tokens = scan_all_tokens stream [] in
  let ast = parse tokens in
  ast\n\n"""

top_fp.write(drivers)

top_fp.close()
