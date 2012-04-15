all: main
	erl -noshell -s main main -s init stop < main.in
	erl -noshell -s main main -s init stop < main2.in
	erl -noshell -s main main -s init stop < main3.in
	erl -noshell -s main main -s init stop < main4.in
	erl -noshell -s main main -s init stop < main5.in
	erl -noshell -s main main -s init stop < main6.in

main: main.erl
	erl -compile main
