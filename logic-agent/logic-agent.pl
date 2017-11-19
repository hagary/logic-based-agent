:- discontiguous agent/3.
:- discontiguous rock/3.
:- discontiguous blank/3.
:- discontiguous obstacle/2.

:- include('initial-state').
:- include('query').
:- use_module(library(clpfd)).


obstacle(-1,-1).
action(up).
action(down).
action(right).
action(left).

next_cell(up, R, C, RNew, CNew):-
  RNew #= R - 1, CNew #= C.
next_cell(down, R, C, RNew, CNew):-
  RNew #= R + 1, CNew #= C.
next_cell(right, R, C, RNew, CNew):-
  CNew #= C + 1, RNew #= R.
next_cell(left, R, C, RNew, CNew):-
  CNew #= C - 1, RNew #= R.

valid(R,C):-
  dimensions(M, N),
  R #< M, C #< N,
  R #>= 0, C #>= 0.

%-----------------------------------------------------------------------------------------------------------
%---------------------------------------ROCK--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------

rock(R, C, result(A,S)):-
  % It was not true & sth made it true.
  \+(obstacle(R,C)),
  valid(R,C),
  action(A),
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

%-----------------------------------------------------------------------------------------------------------
%---------------------------------------AGENT--------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------
agent(R, C, result(A,S)):-
  % It was not true & sth made it true.
  % Next cell is neither a rock nor an obstacle.
  action(A),
  valid(R,C),
  \+obstacle(R,C),
  \+rock(R,C,S),
  next_cell(A, RAgent, CAgent, R, C),
  agent(RAgent, CAgent, S).

agent(R, C, result(A,S)):-
  % It was not true & sth made it true.
  % Next cell is a rock and the agent was able to move it.
  action(A),
  valid(R,C),
  rock(R,C,S),
  next_cell(A, R, C, RNext, CNext),
  valid(RNext, CNext),
  next_cell(A, RAgent, CAgent, R, C),
  valid(RAgent, CAgent),
  \+obstacle(RNext, CNext),
  \+rock(RNext, CNext, S),
  agent(RAgent, CAgent, S).


query_iter(S, D, R):-
  call_with_depth_limit(query(S), D, R),
  R \= depth_limit_exceeded.

query_iter(S, D, R):-
  R = depth_limit_exceeded,
  D1 is D + 1,
  query_iter(S, D1, _).
