all: main
	erl -noshell -s main main -s init stop < main.in

main: main.erl randmat.erl thresh.erl winnow.erl outer.erl product.erl
	erl -compile main randmat thresh winnow outer product
