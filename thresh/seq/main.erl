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
-export([main/0]).
-define(INT_MAX,2147483647).

randvet(0) -> [];
randvet(Ncols) -> [random:uniform(?INT_MAX) | randvet(Ncols - 1)].

randmat_impl(0, _) -> [];
randmat_impl(Nrows, Ncols) -> [randvet(Ncols) | randmat_impl(Nrows - 1, Ncols)].

randmat(Nrows, Ncols, S) -> random:seed(S, S, S),
  randmat_impl(Nrows, Ncols).

main() ->
  {ok, [Nrows, Ncols, S]} = io:fread("","~d~d~d"),
  io:format("~w~n\n", [randmat(Nrows, Ncols, S)]).

