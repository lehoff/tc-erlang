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

-module(randmat).
-export([randmat/3]).
-define(INT_MAX,1000).

randvet_impl(0) -> [];
randvet_impl(Ncols) -> [random:uniform(?INT_MAX) | randvet_impl(Ncols - 1)].

randvet(Ncols, S) ->
  random:seed(S + pid_to_integer(self()), S, S),
  randvet_impl(Ncols).

pid_to_integer(X) ->
  Y = pid_to_list(X),
  W = string:substr(Y, string:str(Y, ".") + 1, string:len(Y)),
  Z = string:substr(W, 1, string:str(W, ".") - 1),
  list_to_integer(Z).

randmat_impl(0, _, _) -> [];
randmat_impl(Nrows, Ncols, S) ->
  Parent = self(),
  % parallel for on rows
  [spawn(fun() -> Parent ! {self(), randvet(Ncols, S)} end) |
    randmat_impl(Nrows - 1, Ncols, S)].

join(Pids) -> [receive {Pid, Result} -> Result end || Pid <- Pids].

randmat(Nrows, Ncols, S) -> join(randmat_impl(Nrows, Ncols, S)).
