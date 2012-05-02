all: main
	erl -noshell -s main main -s init stop < main.in

main: main.erl randmat.erl
	erl -compile main randmat
