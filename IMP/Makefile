MAIN=interp

OBJS = ast.cmo lexer.cmo parser.cmo eval.cmo main.cmo

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<


$(MAIN): $(OBJS)
	ocamlc -o $(MAIN) $(OBJS)

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc -c lexer.ml

parser.ml : parser.mly
	ocamlyacc -q $<

parser.mli : parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)
