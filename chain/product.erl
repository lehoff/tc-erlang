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

-module(product).
-export([product/3]).

join(Pids) -> [receive {Pid, Result} -> Result end || Pid <- Pids].

product(_, Matrix, Vector) ->
  Parent = self(),
  % parallel for on rows
  join([spawn(fun() -> Parent ! {self(),
      lists:sum([ A * B || {A, B} <- lists:zip(L, Vector)])} end)
    || L <- Matrix]).
