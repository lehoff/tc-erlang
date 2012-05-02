%
% thresh: histogram thresholding
%
% input:
%   matrix: the integer matrix to be thresholded
%   nrows, ncols: the number of rows and columns
%   percent: the percentage of cells to retain
%
% output:
%   mask: a boolean matrix filled with true for cells that are kept
%

-module(thresh).
-export([thresh/4]).

reduce2d_worker(Parent, X, Function) ->
  spawn(fun() ->
      Result = Function(X),
      Parent ! {self(), Result}
  end).

reduce2d_join(Pids) ->
  [receive {Pid, Result} -> Result end || Pid <- Pids].

reduce2d(Matrix, Agregator, Function) ->
  Parent = self(),
  Pids = [reduce2d_worker(Parent, X, Function) || X <- Matrix],
  Agregator(reduce2d_join(Pids)).

max_matrix(Matrix) ->
  reduce2d(Matrix, fun lists:max/1, fun lists:max/1).

count_equal(Matrix, Value) ->
  reduce2d(Matrix, fun lists:sum/1,
    fun(X) -> length(lists:filter(fun(Y) -> Y == Value end, X)) end).

fill_histogram(Matrix, 0) -> [count_equal(Matrix, 0)];
fill_histogram(Matrix, Nmax) ->
  [count_equal(Matrix, Nmax) | fill_histogram(Matrix, Nmax - 1)].

get_threshold(-1, [], _) -> 0;
get_threshold(Index, [Head | Tail], Count) ->
  if Count - Head < 0 -> Index;
    true -> get_threshold(Index - 1, Tail, Count - Head)
  end.

filter(Matrix, Threshold) ->
  reduce2d(Matrix, fun(X) -> X end, fun(X) ->
        lists:map(fun(Y) -> Y >= Threshold end, X) end).

thresh(Nrows, Ncols, Matrix, Percent) ->
  Nmax = max_matrix(Matrix),
  Histogram = fill_histogram(Matrix, Nmax),
  Count = (Nrows * Ncols * Percent) / 100,
  Threshold = get_threshold(Nmax, Histogram, Count),
  Mask = filter(Matrix, Threshold),
  Mask.
