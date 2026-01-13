factorial(0,1).
factorial(N,F):-
	N > 0,
	N1 is N-1,
	factorial(N1,F1),
	F is F1*N.

sum_rec(1,1).
sum_rec(N, Sum):-
	N > 1,
	N1 is N - 1,
	sum_rec(N1,Sum1),
	Sum is Sum1 + N.

pow_rec(_,0,1).
pow_rec(X,Y,P):-
	Y > 0,
	Y1 is Y - 1,
	pow_rec(X,Y1,P1),
	P is X * P1.

square_rec(N,S):-
    square_aux(N,N,S).

square_aux(_,0,0).
square_aux(N,C,S):-
    C > 0,
    C1 is C - 1,
    square_aux(N,C1,S1),
    S is S1 + N.

fibonnaci(0,0).
fibonnaci(1,1).
fibonnaci(N,F):-
	N > 1,
	N1 is N -1,
	N2 is N -2,
	fibonnaci(N1,F1),
	fibonnaci(N2,F2),
	F is F1 + F2.
	
collatz(1,0).
collatz(N,S):-
	N > 1,
	0 is N mod 2,
	N1 is N // 2,
	collatz(N1,S1),
	S is S1 + 1.
collatz(N,S):-
	N > 1,
	1 is N mod 2,
	N1 is 3 * N + 1,
	collatz(N1,S1),
	S is S1 + 1.

is_prime(2).
is_prime(X) :-
    X > 2,
    no_divisor(X, 2).
no_divisor(N, D) :-
    D * D > N.
no_divisor(N, D) :-
    D * D =< N,
    N mod D =\= 0,
    D1 is D + 1,
    no_divisor(N, D1).


