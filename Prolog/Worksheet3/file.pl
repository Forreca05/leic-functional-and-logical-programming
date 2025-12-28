max1(A, B, B):- B >= A, !.
max1(A, B, A) :- A > B.

list_size([],0).
list_size([_|T], N):-
    list_size(T,N1),
    N is N1 + 1.

list_sum([],0).
list_sum([H|T], N):-
    list_sum(T,N1),
    N is N1 + H.

list_mul([],0).
list_mul([H|T], N):-
    list_mul(T,N1),
    N is N1 * H.

inner_product([], _, 0).
inner_product(_, [], 0).
inner_product([H1|T1], [H2|T2], N):-
    inner_product(T1,T2,N1),
    N is N1 + H1*H2.

count(_, [], 0).
count(X, [H|T], N) :-
    X == H,
    count(X, T, N1),
    N is N1 + 1.
count(X, [_|T], N) :-
    count(X, T, N).
