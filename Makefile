SOURCES = syntax.ml type.ml eval.ml print.ml myLexer.mll myParser.mly myOCaml.ml
RESULT  = myOCaml

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
