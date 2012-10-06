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

-module(winnow).
-export([main/0]).
-export([winnow/5]).

%% @doc a process for each row, send all the weighted indicies which
%% has mask equal 1 to the Parent process.
row_process(Parent, {RowNo, Columns, Masks}, Ncols) ->
    ColumnData = lists:zip3( Columns,
                             Masks,
                             lists:seq(0, Ncols-1) ),
    Weighted =
        [ {W, {RowNo, ColNo}}
          || {W, true, ColNo} <- ColumnData ],
    Parent ! Weighted.


winnow(Nrows, Ncols, Matrix, Mask, Nelts) ->
    Rows = lists:zip3(lists:seq(0, Nrows-1),
                      Matrix, Mask),
    Parent = self(),
    [ spawn(fun() ->
                    row_process(Parent, Row, Ncols)
            end)
      || Row <- Rows ],
    Results = [ receive
                    Res ->
                        Res
                end
                || _ <- Rows ],
    %%io:format("Results in: ~p~n", [Results]),
    Sorted = lists:sort( lists:flatten(Results) ),
    Masked = no_masked(Mask),
    ToDrop = max(0, Masked - Nelts),
    {_,FinalN} = lists:split(ToDrop, Sorted),
    [ Coord || {_, Coord} <- FinalN ].

no_masked(Mask) ->
    lists:sum(lists:map(fun(true) -> 1;
                           (false) -> 0
                        end,
                        lists:flatten(Mask))
             ).

%%--------------------------------------------
%% reading data
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

