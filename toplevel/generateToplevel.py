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

top_fp.close()
