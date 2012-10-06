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
-export([main/0, main/1]).
-export([randmat/3]).
-export([row_proc/3]).
-define(INT_MAX,2147483647).
-define(RAND_MAX,100).
-define(LCG_A,1664525).
-define(LCG_C,1013904223).

randvet(0, _) -> [];
randvet(Ncols, S) ->
  NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
  [NewS rem ?RAND_MAX | randvet(Ncols - 1, NewS)].

%%join(Pids) -> [receive {Pid, Result} -> Result end || Pid <- Pids].

%% @doc just read all the rows as they are done, do not care about the
%% order since we are creating a random matrix.
join(Pids) ->
    [ receive
          Result ->
              Result
      end
      || _Pid <- Pids
    ].

row_proc(Parent, Ncols, S) ->
    Parent ! randvet(Ncols, S).


%% @doc spawn a process to create each row and then collect all the
%% results. 
randmat(Nrows, Ncols, S) ->
    join( [ spawn(?MODULE, row_proc, [self(), Ncols, S + Row])
            || Row <- lists:seq(1, Nrows) ] ).

main() -> main(['']).
main([is_bench|_]) ->
    '';
main(_Args) ->
    {ok, [Nrows, Ncols, S]} = io:fread("","~d~d~d"),
    Matrix = randmat(Nrows, Ncols, S),
    io:format("~w~n\n", [Matrix]).
