BUILD_PKGS=
TEST_PKGS=oUnit

BUILD_FLAGS=-Is src
DEBUG_FLAGS=-tag 'debug'
TEST_FLAGS=-use-ocamlfind -pkgs ${TEST_PKGS} -Is src

# ^: add -build-dir _build to put .native in _build/
# add -- after .native below to run immediately

default: build

build:
	ocamlbuild ${BUILD_FLAGS} src/main.native
	gcc -c runtime/runtime.c -o runtime/runtime.o

repl:
	ocamlbuild ${BUILD_FLAGS} src/repl.native

debug:
	ocamlbuild ${BUILD_FLAGS} ${DEBUG_FLAGS} src/main.d.byte
	gcc -c runtime/runtime.c -o runtime/runtime.o

doc:
	ocamldoc -html src/*mli -d docs

output:
	gcc -c output.S -o output.o
	gcc -c runtime/runtime.c -o runtime/runtime.o
	gcc output.o runtime/runtime.o -o output

test:
	# ocamlbuild ${BUILD_FLAGS} ${TEST_FLAGS} ${DEBUG_FLAGS} tests/test.d.byte
	# ocamlbuild ${TEST_FLAGS} tests/test.native --
	ocamlbuild ${TEST_FLAGS} tests/exampleTests.native --

clean:
	ocamlbuild -clean
	rm -f docs/* *.native

# Add Jane Street
# opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
