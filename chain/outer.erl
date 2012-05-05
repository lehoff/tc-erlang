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

join(Pids) -> [receive {Pid, Result} -> Result end || Pid <- Pids].

sqr(X) -> X * X.

distance({Ax, Ay}, {Bx, By}) -> math:sqrt(sqr(Ax - Bx) + sqr(Ay - By)).

fix_diagonal_vector(_, _, [], _, _, _) -> [];
fix_diagonal_vector(Line, Col, [Head | Tail], Point, Nelts, Nmax) ->
  if Line == Col -> [Nelts * Nmax | Tail];
    true -> [ Head | fix_diagonal_vector(
          Line, Col + 1, Tail, Point, Nelts, Nmax)]
  end.

fix_diagonal(_, [], _, _) -> [];
fix_diagonal(Line, [Head | Tail], [HeadPoints | TailPoints], Nelts) ->
  Parent = self(),
  % parallel for on rows
  [spawn(fun() -> Parent ! {self(),
            fix_diagonal_vector(Line, 0, Head, HeadPoints, Nelts,
              lists:max(Head))} end) |
    fix_diagonal(Line + 1, Tail, TailPoints, Nelts)].

outer(Nelts, Points) ->
  Parent = self(),
  % parallel for on rows
  {join(fix_diagonal(0, join([spawn(fun() -> Parent ! {self(),
                  [distance(A, B) || A <- Points]} end) || B <- Points]),
      Points, Nelts)),
  [distance({0, 0}, A) || A <- Points]}.
