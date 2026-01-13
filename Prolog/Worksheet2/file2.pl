factorial(N,F):-
	factorial_acc(N,1,F).

factorial_acc(0,A,A).
factorial_acc(N,A,F):-
	N > 0,
	N1 is N - 1,
	A1 is A * N,
	factorial_acc(N1,A1,F).

sum_rec(N,S):-
	sum_rec_aux(N,0,S).

sum_rec_aux(0,A,A).
sum_rec_aux(N,A,F):-
	N>0,
	N1 is N-1,
	A1 is A + N,
	sum_rec_aux(N1,A1,F).

fibonacci(N, F) :-
    fibonacci_acc(N, 0, 1, F).

fibonacci_acc(0, A, _, A).
fibonacci_acc(N, A, B, F) :-
    N > 0,
    N1 is N - 1,
    Sum is A + B,
    fibonacci_acc(N1, B, Sum, F).

gcd(X, 0, X) :- 
    X > 0.
gcd(X, Y, G) :-
    Y > 0,
    Z is X mod Y,
    gcd(Y, Z, G).
