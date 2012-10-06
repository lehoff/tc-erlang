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
-export([compute/0]).
-import(randmat, [randmat/3]).
-import(thresh, [thresh/4]).
-import(winnow, [winnow/5]).
-import(outer, [outer/2]).
-import(product, [product/3]).

main() ->
    {Time, Value} = timer:tc(?MODULE, compute, []),  
    io:format(standard_error,
              "Time: ~p~n",
              [Time]).



compute() ->
    {ok, [Nelts, RandmatSeed, ThreshPercent, WinnowNelts]} =
        io:fread("","~d~d~d~d"),
    RandmatMatrix = randmat:randmat(Nelts, Nelts, RandmatSeed),
    %% io:format("RandmatMatrix: ~p~n",
    %%           [RandmatMatrix]),
    ThreshMask = thresh:thresh(Nelts, Nelts, RandmatMatrix, ThreshPercent),
    %% io:format("ThreshMask: ~p~n", [ThreshMask]),
    WinnowPoints = winnow:winnow(Nelts, Nelts, RandmatMatrix, ThreshMask,
      WinnowNelts),
    {OuterMatrix, OuterVector} = outer:outer(WinnowNelts, WinnowPoints),
    ProductResult = product:product(WinnowNelts, OuterMatrix, OuterVector),
    io:format("~w~n", [ProductResult]).
