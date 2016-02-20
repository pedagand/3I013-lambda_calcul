all : lambda_calcul

lambda_calcul: lambda.ml
	ocamlbuild -use-ocamlfind lambda.native

test.native: test.ml
	ocamlbuild -use-ocamlfind test.native

clean: 
	ocamlbuild -clean
