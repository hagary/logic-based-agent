%----------------------- HOW TO QUERY? -----------------------------------------
% query_iter(S, 1, R).
%-------------------------------------------------------------------------------

:- discontiguous agent/3.
:- discontiguous rock/3.
:- discontiguous obstacle/2.
:- discontiguous pad/2.

%--------------------------Imports----------------------------------------------
:- include('initial-state-Sol1').
:- include('query-Sol1').
:- use_module(library(clpfd)).
%-------------------------------------------------------------------------------


obstacle(-1,-1). % To eliminate errors in case no obstacle is present.

%------------------------Helper Predicates--------------------------------------

% **next_cell** works both ways to get the position of the neighbouring cell in the direction of a given action.
next_cell(up, R, C, RNext, CNext):-
  RNext #= R - 1, CNext #= C.
next_cell(down, R, C, RNext, CNext):-
  RNext #= R + 1, CNext #= C.
next_cell(right, R, C, RNext, CNext):-
  CNext #= C + 1, RNext #= R.
next_cell(left, R, C, RNext, CNext):-
  CNext #= C - 1, RNext #= R.

% **valid** checks that a given cell position is within the dimensions of the grid.
valid(R,C):-
  dimensions(M, N),
  R #< M, C #< N,
  R #>= 0, C #>= 0.
%-------------------------------------------------------------------------------

%--------------------------------ROCK-------------------------------------------
rock(R, C, result(A,S)):-
  % It was not true & sth made it true.
  \+(obstacle(R,C)),
  valid(R,C),
  next_cell(A, ROld, COld, R, C),
  next_cell(A, RAgent, CAgent, ROld, COld),
  \+(rock(R,C,S)),
  rock(ROld, COld, S),
  agent(RAgent, CAgent, S).

rock(R, C, result(A,S)):-
  % It was true & nth affected it.
   rock(R,C,S),
   next_cell(A, RAgent, CAgent, R, C),
    %If the agent was in an adjacent cell, then its action was not valid.
    (\+agent(RAgent, CAgent, S);
    (
    next_cell(A, R, C, RNew, CNew),
    (
      % Rock is next to an edge.
      \+(valid(RNew, CNew));
      % Rock is next to an obstacle.
      obstacle(RNew, CNew);
      % Rock next to another rock.
      rock(RNew, CNew, S)
    ))).
%-------------------------------------------------------------------------------

%-------------------------------AGENT-------------------------------------------
agent(R, C, result(A,S)):-
  % It was not true & sth made it true.
  valid(R,C),
  next_cell(A, RAgent, CAgent, R, C),
  % Aspired cell is neither a rock nor an obstacle.
  ((
    \+obstacle(R,C),
    \+rock(R,C,S)
  );
  % Aspired cell is a movable rock.
  (
  next_cell(A, R, C, RNext, CNext),
  valid(RNext, CNext),
  \+obstacle(RNext, CNext),
  rock(R,C,S),
  \+rock(RNext, CNext, S)
  )),
  agent(RAgent, CAgent, S).


agent(R, C, result(A,S)):-
  % It was true and nth affected it.
  agent(R, C, S),
  next_cell(A, R, C, RNext, CNext),
  (
  % Next cell is an obstacle.
  obstacle(RNext, CNext)
  ;
  (
  % Next cell is an unmovable rock.
  rock(RNext, CNext, S),
  next_cell(A, RNext, CNext, RNext2, CNext2),
  (obstacle(RNext2, CNext2) ; rock(RNext2, CNext2, S))
  )).
%-------------------------------------------------------------------------------

query_iter(S, D, R):-
  call_with_depth_limit(query(S), D, R),
  R \= depth_limit_exceeded.

query_iter(S, D, _):-
  D1 is D + 1,
  query_iter(S, D1, _).

%----------------------------------THE END--------------------------------------
