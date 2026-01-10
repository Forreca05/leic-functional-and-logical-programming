double(X, Y):- Y is X*2.
map(_,[],[]).
map(F,[H|T],[L|Rest]):-
    call(F,H,L),
    map(F,T,Rest).

sum(A, B, S):- S is A+B.
fold(_, S, [], S).
fold(F, S, [H|T], M):-
    call(F,S,H,L),
    fold(F,L,T,M).

even(X):- 0 =:= X mod 2.
separate([],_,[],[]).
separate([H|T],F,[H|A],B):-
	call(F,H),
	separate(T,F,A,B).
separate([H|T],F,A,[H|B]):-
	\+ call(F,H),
	separate(T,F,A,B).

take_while(_,[],[],[]).
take_while(F,[H|T],[H|A1],B):-
	call(F,H),
	take_while(F,T,A1,B).
take_while(F,[H|T],[],[H|T]):-
	\+ call(F,H).
	
ask_execute:-
	write('Insert the goal to execute'),
	read(Goal),
	call(Goal).


% Caso 1: extrair functor e aridade de um termo
my_functor(Term, F, N) :-
    Term =.. [F | Args],
    length(Args, N).

% Caso 2: construir um termo a partir de functor e aridade
my_functor(Term, F, N) :-
    N >= 0,
    length(Args, N),
    Term =.. [F | Args].

find_list(1,[H|_],H):- !.
find_list(N,[_|T],Arg):-
	N > 1,
	N1 is N - 1,
	find_list(N1,T,Arg).
my_arg(N,G,Arg):-
	G =.. [_| Args],
	find_list(N,Args,Arg).

tree_size(null,0).
tree_size(node(_,Left,Right),S):-
	tree_size(Left,S1),
	tree_size(Right,S2),
	S is S1 + S2 +1.