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

-module(main).
-export([main/0]).

max(A, B) ->
  if A > B -> A;
    true -> B
  end.

max_vector([]) -> 0;
max_vector([Head | Rest]) -> max(Head, max_vector(Rest)).

max_matrix(_, _, []) -> 0;
max_matrix(_, _, [Head | Rest]) -> max(max_vector(Head),
    max_matrix(0, 0, Rest)).

fill_histogram(Nrows, Ncols, Matrix, Nmax) -> [0].

get_threshold(Histogram, Count) -> 0.

filter(Nrows, Ncols, Matrix, Threshold) -> [[]].

thresh(Nrows, Ncols, Matrix, Percent) ->
  Nmax = max_matrix(Nrows, Ncols, Matrix),
  Histogram = fill_histogram(Nrows, Ncols, Matrix, Nmax),
  Count = (Nrows * Ncols * Percent) / 100,
  Threshold = get_threshold(Histogram, Count),
  Mask = filter(Nrows, Ncols, Matrix, Threshold),
  Mask.

read_vector(0) -> [];
read_vector(Ncols) -> {ok, [Value]} = io:fread("", "~d"),
  [ Value | read_vector(Ncols - 1)].

read_matrix(0, _) -> [];
read_matrix(Nrows, Ncols) -> [read_vector(Ncols) |
    read_matrix(Nrows - 1, Ncols)].

main() ->
  {ok, [Nrows, Ncols]} = io:fread("","~d~d"),
  Matrix = read_matrix(Nrows, Ncols),
  {ok, [Percent]} = io:fread("", "~d"),
  io:format("~w~n\n", [thresh(Nrows, Ncols, Matrix, Percent)]).

