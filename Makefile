ARCHIVE_NAME=Lorentz

archive:
	@rm --force $(ARCHIVE_NAME).tar*
	git archive master --output $(ARCHIVE_NAME).tar --prefix=$(ARCHIVE_NAME)/
	gzip $(ARCHIVE_NAME).tar
all: 
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native impterpreter

byte: 
	ocamlbuild -yaccflag -v main.byte

tests: all
	./tests/run.sh

run: all
	./impterpreter

clean: 
	ocamlbuild -clean
