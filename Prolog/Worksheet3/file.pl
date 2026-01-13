max1(A, B, B):- B >= A, !.
max1(A, B, A):- A > B.

list_size([],0).
list_size([_|T],N):-
	list_size(T,N1),
	N is N1 + 1.

list_sum([],0).
list_sum([X|T],N):-
	list_sum(T,N1),
	N is N1 + X.

list_prod([],0).
list_prod([X|T],N):-
	list_prod(T,N1),
	N is N1 * X.

inner_product([],_,0).
inner_product(_,[],0).
inner_product([H1|T1],[H2|T2],N):-
	inner_product(T1,T2,N1),
	N is N1 + H1*H2.

count(_,[],0).
count(X,[H|T],C):-
	X==H,
	count(X,T,C1),
	C is C1 + 1.
count(X,[_|T],C):-
	count(X,T,C).

del_one(_,[],[]).
del_one(X,[X|T],T).
del_one(X, [H|T], [H|R]) :-
    X \= H,
    del_one(X, T, R).

del_all(_,[],[]).
del_all(X,[X|T],Y):-
	del_all(X,T,Y).
del_all(X,[H|T],[H|R]):-
	X\=H,
	del_all(X,T,R).

del_all_list([], H, H).
del_all_list([X|T], H, Y) :-
    del_all(X, H, H1),
    del_all_list(T, H1, Y).

del_dups([],[]).
del_dups([X|T],[X|R]):-
	del_all(X,T,Y),
	del_dups(Y,R).

is_ordered([]).
is_ordered([_]).
is_ordered([A,B|T]) :-
    integer(A),
    integer(B),
    A =< B,
    is_ordered([B|T]).

insert_ordered(X,[],[X]).
insert_ordered(X,[H|T],R):-
	integer(X),
	integer(H),
	X =< H,
	R = [X,H|T].
insert_ordered(X,[H|T],[H|R]):-
	integer(X),
	integer(H),
	insert_ordered(X,T,R).

insert_sort([], []).
insert_sort([X|T], Sorted) :-
    insert_sort(T, SortedT),
    insert_ordered(X, SortedT, Sorted).
