%
% randmat: random number generation
%
% input:
%   nrows, ncols: the number of rows and columns
%   s: the seed
%
% output:
%   matrix: a nrows x ncols integer matrix
%

-module(main).
-export([main/0, main/1]).
-define(INT_MAX,2147483647).
-define(RAND_MAX,100).
-define(LCG_A,1664525).
-define(LCG_C,1013904223).

randvet(0, _) -> [];
randvet(Ncols, S) ->
  NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
  [NewS rem ?RAND_MAX | randvet(Ncols - 1, NewS)].

randmat(0, _, _) -> [];
randmat(Nrows, Ncols, S) ->
  [randvet(Ncols, S) | randmat(Nrows - 1, Ncols, S + 1)].

main() -> main(['']).
main(Args) ->
  [Head | _] = Args,
  IsBench = string:equal(Head, 'is_bench'),
  {ok, [Nrows, Ncols, S]} = io:fread("","~d~d~d"),
  Matrix = randmat(Nrows, Ncols, S),
  case IsBench of false -> io:format("~w~n\n", [Matrix]); true -> ''
  end.
