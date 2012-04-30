all: main
	erl -noshell -s main main -s init stop < main.in
	erl -noshell -s main main -s init stop < main.in > main.out
	diff main.out main.gold
	erl -noshell -s main main -s init stop < main2.in > main2.out
	diff main2.out main2.gold
	erl -noshell -s main main -s init stop < main3.in > main3.out
	diff main3.out main3.gold
	erl -noshell -s main main -s init stop < main4.in > main4.out
	diff main4.out main4.gold

main: main.erl
	erl -compile main
