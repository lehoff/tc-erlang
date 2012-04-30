%
% winnow: weighted point selection
%
% input:
%   matrix: an integer matrix, whose values are used as masses
%   mask: a boolean matrix showing which points are eligible for
%     consideration
%   nrows, ncols: the number of rows and columns
%   nelts: the number of points to select
%
% output:
%   points: a vector of (x, y) points
%

-module(main).
-export([main/0]).

get_values_vector(_, _, [], []) -> [];
get_values_vector(Line, Col, [ValuesHead | ValuesTail], [
    MaskHead | MaskTail]) ->
  Rest = get_values_vector(Line, Col + 1, ValuesTail, MaskTail),
  if MaskHead == 1 -> [
        {ValuesHead, {Line, Col}} | Rest];
    true -> Rest
  end.

get_values(_, [], []) -> [];
get_values(Line, [MatrixHead | MatrixTail], [MaskHead | MaskTail]) ->
  get_values_vector(Line, 0, MatrixHead, MaskHead) ++ get_values(
    Line + 1, MatrixTail, MaskTail).

drop(L, 0) -> L;
drop([_ | T], I) -> drop (T, I - 1).

get_points(0, _, _) -> [];
get_points(Nelts, [{_, {I, J}} | Tail], Chunk) ->
  [ {I, J} | get_points(Nelts - 1, drop(Tail, Chunk - 1), Chunk)].

winnow(_, _, Matrix, Mask, Nelts) ->
  Values = get_values(0, Matrix, Mask),
  Sorted = lists:sort(Values),
  N = length(Sorted),
  Chunk = N div Nelts,
  Points = get_points(Nelts, Sorted, Chunk),
  [Points].

read_vector(0) -> [];
read_vector(Ncols) -> {ok, [Value]} = io:fread("", "~d"),
  [ Value | read_vector(Ncols - 1)].

read_matrix(0, _) -> [];
read_matrix(Nrows, Ncols) -> [read_vector(Ncols) |
    read_matrix(Nrows - 1, Ncols)].

main() ->
  {ok, [Nrows, Ncols]} = io:fread("","~d~d"),
  Matrix = read_matrix(Nrows, Ncols),
  Mask = read_matrix(Nrows, Ncols),
  {ok, [Nelts]} = io:fread("", "~d"),
  io:format("~w~n\n", [winnow(Nrows, Ncols, Matrix, Mask, Nelts)]).

