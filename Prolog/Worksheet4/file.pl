max(A,B,C,Max):-
    integer(A), integer(B), integer(C),
    A >= B, A >= C, !,
    Max is A.

max(_A,B,C,Max):-
    B >= C, !,
    Max is B.

max(_A,_B,C,C).

print_n(0, _).
print_n(N, S) :-
    N > 0,
    write(S),
    N1 is N - 1,
    print_n(N1, S).

writethat([]).
writethat([H|T]):-
	char_code(C,H),
	put_char(C),
	writethat(T).

print_text(T,S,P):-
	print_n(1,S),
	print_n(P,' '),
	writethat(T),
	print_n(P,' '),
	print_n(1,S).

length_line(T,S,P,L):-
	length(T,Size),
	atom_length(S,Size2),
	P2 is 2 * P,
	L is P2 + Size + Size2.

print_banner(T,S,P):-
	length_line(T,S,P,L),
	print_n(L,S),
	nl,
	print_n(1,S),
	L1 is L - 2,
	print_n(L1,' '),
	print_n(1,S),
	nl,
	print_text(T,S,P),
	nl,
	print_n(1,S),
	print_n(L1,' '),
	print_n(1,S),
	nl,
	print_n(L,S).

read_string([]) :-
    peek_code(10), 
    !.
read_string([C|Cs]) :-
    get_code(C),   
    C \= 10,    
    read_string(Cs).


print_full_list([]) :-
    nl.
print_full_list([X]) :-     
    write(X),
    nl.
print_full_list([X|Xs]) :-
    write(X),
    write(', '),      
    print_full_list(Xs).

take(N, L, Prefix) :-
    length(Prefix, N),
    append(Prefix, _, L).
take_last(N, L, Suffix) :-
    length(L, Len),
    Start is Len - N,
    Start >= 0,
    length(Prefix, Start),
    append(Prefix, Suffix, L).
middle_three(L, Middle) :-
    length(L, Len),
    Len >= 12,
    MidStart is Len // 2 - 1,   
    length(Prefix, MidStart),
    append(Prefix, Rest, L),
    length(Middle, 3),
    append(Middle, _, Rest).
print_list(L) :-
    length(L, Len),
    Len =< 11,
    write('['),
    print_full_list(L),
    write(']').
print_list(L) :-
    length(L, Len),
    Len >= 12,
    take(3, L, First),
    take_last(3, L, Last),
    middle_three(L, Middle),
    write('['),
    print_full_list(First),
    write(', ..., '),
    print_full_list(Middle),
    write(', ..., '),
    print_full_list(Last),
    write(']').

print_matrix([]).
print_matrix([H|T]):-
	write(H),
	nl,
	print_matrix(T).