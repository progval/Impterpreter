all: 
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native impterpreter

byte: 
	ocamlbuild -yaccflag -v main.byte

tests: all
	./tests/run.sh

clean: 
	ocamlbuild -clean
