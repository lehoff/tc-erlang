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
-export([winnow/5]).

get_values_vector(_, _, [], []) -> [];
get_values_vector(Line, Col,
                  [ValuesHead | ValuesTail],
                  [MaskHead | MaskTail]) ->
    Rest = get_values_vector(Line, Col + 1, ValuesTail, MaskTail),
    case MaskHead of
        1 ->
            [{ValuesHead, {Line, Col}} | Rest];
        _ -> Rest
    end.


join(Pids) ->
  [receive {Pid, Result} -> Result end || Pid <- Pids].

% worker, processes and sends the results back
get_values_worker(Parent, Line, Col, Values, Mask) ->
  spawn(fun() ->
        Result = get_values_vector(Line, Col, Values, Mask),
        Parent ! {self(), Result}
  end).

get_values_impl(_, _, [], []) -> [];
get_values_impl(Parent, Line,
                [MatrixHead | MatrixTail],
                [MaskHead | MaskTail]) ->
  % parallel for on rows
  [get_values_worker(Parent, Line, 0, MatrixHead, MaskHead) |
    get_values_impl(Parent, Line + 1, MatrixTail, MaskTail)].

append([]) -> [];
append([Head | Tail]) -> Head ++ append(Tail).

get_values(Line, Matrix, Mask) ->
  Parent = self(),
  Pids = get_values_impl(Parent, Line, Matrix, Mask),
  Results = append(join(Pids)),
  Results.

drop(L, 0) -> L;
drop([_ | T], I) -> drop (T, I - 1).

get_points(0, _, _) -> [];
get_points(Nelts, [{_, {I, J}} | Tail], Chunk) ->
  [ {I, J} | get_points(Nelts - 1, drop(Tail, Chunk - 1), Chunk)].

sort_impl(L) ->
  Parent = self(),
  % parallel for on L
  join([ spawn(fun() -> Parent ! {self(), sort(X)} end) || X <- L ]).

sort([]) -> [];
sort([Pivot | Tail]) ->
  Left = [X || X <- Tail, X < Pivot],
  Right = [X || X <- Tail, X >= Pivot],
  Results = sort_impl([Left, Right]),
  hd(Results) ++ [Pivot] ++ hd(tl(Results)).

winnow(_, _, Matrix, Mask, Nelts) ->
  Values = get_values(0, Matrix, Mask),
  Sorted = sort(Values),
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
    
    {Time, Res} = timer:tc(?MODULE, winnow,
                           [Nrows, Ncols, Matrix, Mask, Nelts]),
    io:format(standard_error, "time: ~p~n", [Time]),
    io:format("~w~n\n", [Res]).

