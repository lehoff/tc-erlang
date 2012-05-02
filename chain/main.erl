%
% chain: chain all problems
%
% input:
%   nelts: the number of elements
%   randmat_seed: random number generator seed
%   thresh_percent: percentage of cells to retain
%   winnow_nelts: the number of points to select
%
% output:
%   result: a real vector, whose values are the result of the final product
%

-module(main).
-export([main/0]).
-import(randmat, [randmat/3]).

main() ->
  {ok, [Nelts, RandmatSeed]} = io:fread("","~d~d"),
  io:format("~w~n", [randmat:randmat(Nelts, Nelts, RandmatSeed)]).

