-module(test).
-export([fac/1]).
-export([main/0]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

main() ->
  io:format("~w~n\n", [fac(10)]).
