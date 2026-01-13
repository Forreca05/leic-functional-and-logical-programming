%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Missionaries and Cannibals
%  State representation:
%     state(ML, CL, MR, CR, BoatSide)
%
%  ML = missionários na margem esquerda
%  CL = canibais na margem esquerda
%  MR = missionários na margem direita
%  CR = canibais na margem direita
%  BoatSide = left | right
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safe condition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state(_,_,_,_,_).
move(_,_,_,_).
safe(M, C) :- M >= C ; M = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Possible moves
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move(2,0).   % 2 missionários
move(0,2).   % 2 canibais
move(1,1).   % 1 missionário + 1 canibal
move(1,0).   % 1 missionário
move(0,1).   % 1 canibal

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% State transitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Boat goes from left to right
transition(state(ML,CL,MR,CR,left),
           state(ML2,CL2,MR2,CR2,right),
           M, C) :-
    move(M,C),
    ML2 is ML - M, CL2 is CL - C,
    MR2 is MR + M, CR2 is CR + C,
    safe(ML2,CL2), safe(MR2,CR2).

% Boat goes from right to left
transition(state(ML,CL,MR,CR,right),
           state(ML2,CL2,MR2,CR2,left),
           M, C) :-
    move(M,C),
    ML2 is ML + M, CL2 is CL + C,
    MR2 is MR - M, CR2 is CR - C,
    safe(ML2,CL2), safe(MR2,CR2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DFS search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(State, State, _, []).

solve(State, Goal, Visited,
      [move(M,C,State,Next) | Moves]) :-
    transition(State, Next, M, C),
    \+ member(Next, Visited),
    solve(Next, Goal, [Next|Visited], Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

missionaries_and_cannibals(Moves) :-
    Initial = state(3,3,0,0,left),
    Goal    = state(0,0,3,3,right),
    solve(Initial, Goal, [Initial], Moves).
