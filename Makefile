all : lambda_calcul

lambda_calcul: lambda_calcul.ml
	ocamlc -o lambda_calcul lambda_calcul.ml

clean: 
	rm lambda_calcul.cmi lambda_calcul.cmo lambda_calcul
