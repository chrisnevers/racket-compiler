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
3. ~~Control Flow~~
    - Booleans and relational operators
    - Logical `and` and `not` operators
    - `if-else` expressions

TODO:
* Vectors
* Garbage Collection
* Functions
* ...