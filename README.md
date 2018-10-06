# racket-compiler

## Building The Project
First, ensure you have `ocaml`, `opam`, and `ocamlbuild` installed.

Then:

        cd {project_dir}
        make

Run:

        ./main.native {file_name}


## Implementation List

1. ~~Basics~~
    - `let-in` expressions
    - Addition and subtraction
    - Reads numbers from stdin
    - Prints programs result to stdout
    - Generates x86 and compiles with `GCC`
2. ~~Register Allocation~~
    - Uncovers live variables
    - Builds an interference graph
    - Allocates registers
        - Move Biasing
3. ~~Control Flow~~
    - Boolean support: `#t` and `#f`
    - Relational operators: `eq?`, `<`, `<=`, `>`, `>=`, `pos?`, `zero?`, and `neg?`
    - Logical operators: `and`, `or`, and `not`
    - `if-else`, `when`, `unless`, and `begin` expressions
4. ~~Tuples & Garbage Collection~~
    - Vectors (tuples): `vector`
    - Vector operations: `vector-set!`, `vector-ref`
    - Stop and Copy garbage collection

TODO:
* Functions
* ...