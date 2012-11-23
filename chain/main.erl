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
-export([compute/4]).
-import(randmat, [randmat/3]).
-import(thresh, [thresh/4]).
-import(winnow, [winnow/5]).
-import(outer, [outer/2]).
-import(product, [product/3]).

main() ->
    {ok, [Nelts, RandmatSeed, ThreshPercent, WinnowNelts]} =
        io:fread("","~d~d~d~d"),
    {Time, Value} = timer:tc(?MODULE, compute,
                             [Nelts, RandmatSeed,
                              ThreshPercent, WinnowNelts]),  
    io:format(standard_error,
              "Time: ~p~n Res: ~p~n",
              [Time, Value]).

compute(Nelts, RandmatSeed, ThreshPercent,
        WinnowNelts) ->
    RandmatMatrix = randmat:randmat(Nelts, Nelts, RandmatSeed),
    ThreshMask = thresh:thresh(Nelts, Nelts, RandmatMatrix, ThreshPercent),
    WinnowPoints = winnow:winnow(Nelts, Nelts, RandmatMatrix, ThreshMask,
      WinnowNelts),
    {OuterMatrix, OuterVector} = outer:outer(WinnowNelts, WinnowPoints),
    product:product(WinnowNelts, OuterMatrix, OuterVector).
