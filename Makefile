
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native

santa:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild matchmaking.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph1.txt 1 2 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

clean:
	-rm -rf _build/
	-rm ftest.native
