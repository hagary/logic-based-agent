:- include('initial-state').
:- include('query').
:- use_module(library(clpfd)).

action(u).
action(d).
action(r).
action(l).
obstacle(-1,-1).

next_cell(u, R, C, RNew, CNew):-
  RNew #= R - 1, CNew #= C.
next_cell(d, R, C, RNew, CNew):-
  RNew #= R + 1, CNew #= C.
next_cell(r, R, C, RNew, CNew):-
  CNew #= C + 1, RNew #= R.
next_cell(l, R, C, RNew, CNew):-
  CNew #= C - 1, RNew #= R.

valid(R,C):-
  dimensions(M, N),
  R #< M, C #< N,
  R #>= 0, C #>= 0.

rock(R, C, result(A,S)):-
  % It was not true & sth made it true.
  (
  \+(obstacle(R,C)),
  valid(R,C),
  action(A),
  next_cell(A, ROld, COld, R, C),
  next_cell(A, RAgent, CAgent, ROld, COld),
  \+(rock(R,C,S)),
  rock(ROld, COld, S),
  agent(RAgent, CAgent, S)
  )
  ;
  % It was true & nth affected it.
  (
  rock(R,C,S),
    agent(RAgent, CAgent, S),
    %If the agent was in an adjacent cell, then its action was \+ valid.
    (\+(next_cell(A, RAgent, CAgent, R, C));
    (
    next_cell(A, R, C, RNew, CNew),
    (
      % Rock is next to an edge.
      \+(valid(RNew, CNew));
      % Rock is next to an obstacle.
      obstacle(RNew, CNew);
      % Rock next to another rock.
      rock(RNew, CNew, S)

    )))
  ).

agent(R, C, result(A,S)):-
  % It was not true & sth made it true.
  \+(obstacle(R,C)),
  valid(R,C),
  action(A),
  next_cell(A, RAgent, CAgent, R, C),
  agent(RAgent, CAgent, S),
  \+(rock(R,C, result(A,S))).

query_iter(S, D, R):-
  call_with_depth_limit(query(S), D, R),
  R \= depth_limit_exceeded.

query_iter(S, D, R):-
  D1 is D + 1,
  query_iter(S, D1, R1).
