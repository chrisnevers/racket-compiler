# racket-compiler

## Building The Project
First, ensure you have `ocaml`, `opam`, and `ocamlbuild` installed.

Then:

        cd {project_dir}
        make

Run:

        ./main.native {file_name}

## Testing The Project

Run unit tests:

        make test

Run example tests:

        make ex

## Implementation List

1. ~~Basics~~
    - `let-in` expressions
    - Int operators: `+`, `-`, `*`, `/`, `%`
    - Reads numbers from stdin: `read`
    - Prints values of any type: `print`
    - Prints programs' result to stdout
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
    - `while` loops
4. ~~Tuples & Garbage Collection~~
    - Vectors (tuples): `vector`
    - Vector operations: `vector-set!`, `vector-ref`, `vector-length`
    - Stop and Copy garbage collection
5. ~~Functions~~
    - First class : `(vector foo #t)`
    - Mutually recursive
    - Typed: `(define (add1 [x : Int]) : Int ...)`
6. ~~Closures~~
    - Typed: `(let ([y 4]) (lambda ([z : Int]) : Int (+ y z)))`
7. ~~Arrays~~
    - `(array 1 2 3)`
    - `(array-set! {array} {index} {exp})`
    - Runtime bound checking
8. ~~Chars~~
9. ~~Polymorphism~~
    - Type lambdas: `(Lambda A M)`
9. Type Constructors
    - User defined sum types
    - Polymorphic `(define-type List A [Nil A] [Cons (Vector A List)])`
    - Case expressions

TODO:
* Macros
* Exceptions
* Bytecode & VM
* LLVM
* ...