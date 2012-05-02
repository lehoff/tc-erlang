all: main
	erl -noshell -s main main -s init stop < main.in > main.out
	diff main.out main.gold

main: main.erl
	erl -compile main
