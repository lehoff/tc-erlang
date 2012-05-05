%
% product: a matrix-vector product
%
% input:
%   nelts: the number of elements
%   matrix: a real matrix
%   vector: a real vector
%
% output:
%   result: a real vector, whose values are the result of the product
%

-module(main).
-export([main/0]).

join(Pids) ->
  [receive {Pid, Result} -> Result end || Pid <- Pids].

product(_, Matrix, Vector) ->
  Parent = self(),
  % parallel for on rows
  join([spawn(fun() -> Parent ! {self(),
      lists:sum([ A * B || {A, B} <- lists:zip(L, Vector)])} end)
    || L <- Matrix]).

read_vector(0) -> [];
read_vector(Nelts) -> {ok, [X]} = io:fread("", "~f"),
  [ X | read_vector(Nelts - 1)].

read_matrix(0, _) -> [];
read_matrix(Nelts, Total) ->
  [ read_vector(Total) | read_matrix(Nelts - 1, Total)].

main() ->
  {ok, [Nelts]} = io:fread("","~d"),
  Matrix = read_matrix(Nelts, Nelts),
  Vector = read_vector(Nelts),
  io:format("~w~n\n", [product(Nelts, Matrix, Vector)]).

