.PHONY: pack all run clean
GENERATED=src/parser.mli src/parser.ml src/lexer.ml src/bruin.ml src/breductor.ml
OCAMLC=ocamlc
PACKAGE=hw.zip

ifeq ($(OS),Windows_NT)
	DEL=del /f
else
	DEL=rm -f
endif

all: main.exe

run:
	./main.exe

compParser:
	cd src && $(OCAMLC) $(GENERATED:src/%=%)

compGrammar:
	cd src && $(OCAMLC) -c grammar.ml

main.exe: $(GENERATED) compGrammar compParser
	cd src && $(OCAMLC) grammar.ml $(GENERATED:src/%=%) main.ml -o ../main.exe

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src

$(GENERATED): src/lexer.mll src/parser.mly
	ocamllex src/lexer.mll
	ocamlyacc src/parser.mly

clean:
	cd src && $(DEL) camlprog.exe *.c* *.o*
	$(DEL) main.exe $(PACKAGE)