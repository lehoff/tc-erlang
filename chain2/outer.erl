%
% outer: outer product
%
% input:
%   points: a vector of (x, y) points
%   nelts: the number of points
%
% output:
%   matrix: a real matrix, whose values are filled with inter-point
%     distances
%   vector: a real vector, whose values are filled with origin-to-point
%     distances
%

-module(outer).
-export([outer/2]).

join(Pids) ->
    [receive
         {Pid, Result} ->
             Result
     end
     || Pid <- Pids].

sqr(X) ->
  X * X.

distance({Ax, Ay}, {Bx, By}) ->
  math:sqrt(sqr(Ax - Bx) + sqr(Ay - By)).

%% @doc calculate all distance pairs and then put the Max times Nelts
%% into the RowN element instead of the 0 there.
calc_row(Parent, Nelts, {RowN, RowPairs}) ->
    Dist = [ distance(X,Y) || {X,Y} <- RowPairs ],
    Max = lists:max(Dist),
    {Pre, [_|Post]} = lists:split(RowN - 1, Dist),
    Parent ! {self(), Pre ++ [Max*Nelts | Post]}.

%% @doc prepare the data into rows of the correct {Xi,Xj} points pairs
%% and then calculate each row on its own before collecting all the
%% results. 
outer(Nelts, Points) ->
    io:format("Nelts: ~p~n, Points: ~p~n",
              [Nelts, Points]),
    Parent = self(),
    Pairs = [[ {A,B} || A <-Points] || B <- Points],
    RowAndPairs = lists:zip(lists:seq(1,Nelts), Pairs),
    Pids = [spawn(fun() -> calc_row(Parent, Nelts, Row) end)
            || Row <- RowAndPairs ],
    {join(Pids),
     [distance({0, 0}, A) || A <- Points]}.

read_vector_of_points(0) -> [];
read_vector_of_points(Nelts) -> {ok, [X, Y]} = io:fread("", "~d~d"),
  [ {X, Y} | read_vector_of_points(Nelts - 1)].

main() ->
  {ok, [Nelts]} = io:fread("","~d"),
  Points = read_vector_of_points(Nelts),
  io:format("~w~n\n", [outer(Nelts, Points)]).

