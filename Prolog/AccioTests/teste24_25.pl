:-use_module(library(lists)).

%author(AuthorID, Name, YearOfBirth, CountryOfBirth).
author(1,
       'John Grisham', 1955, 'USA').
author(2,
       'Wilbur Smith', 1933, 'Zambia').
author(3,
       'Stephen King', 1947, 'USA').
author(4,
       'Michael Crichton', 1942, 'USA').

%book(Title, AuthorID, YearOfRelease, Pages, Genres).
book('The Firm',
     1, 1991, 432, ['Legal thriller']).
book('The Client',
     1, 1993, 422, ['Legal thriller']).
book('The Runaway Jury',
     1, 1996, 414, ['Legal thriller']).
book('The Exchange',
     1, 2023, 338, ['Legal thriller']).

book('Carrie',
     3, 1974, 199, ['Horror']).
book('The Shining',
     3, 1977, 447, ['Gothic novel', 'Horror', 'Psychological horror']).
book('Under the Dome',
     3, 2009, 1074, ['Science fiction', 'Political']).
book('Doctor Sleep',
     3, 2013, 531, ['Horror', 'Gothic', 'Dark fantasy']).

book('Jurassic Park',
     4, 1990, 399, ['Science fiction']).
book('Prey',
     4, 2002, 502, ['Science fiction', 'Techno-thriller', 'Horror', 'Nanopunk']).
book('Next',
     4, 2006, 528, ['Science fiction', 'Techno-thriller', 'Satire']).

gives_gift_to(bernardete,'The Exchange',celestina).
gives_gift_to(celestina, 'The Brethren', eleuterio).
gives_gift_to(eleuterio, 'The Summons', felismina).
gives_gift_to(felismina, 'River God', juvenaldo).
gives_gift_to(juvenaldo, 'Seventh Scroll', leonilde).
gives_gift_to(leonilde, 'Sunbird', bernardete).
gives_gift_to(marciliano, 'Those in Peril', nivaldo).
gives_gift_to(nivaldo, 'Vicious Circle', sandrino).
gives_gift_to(sandrino, 'Predator', marciliano).

%Exercise 1
book_author(Title,Author):-
	book(Title,AuthorID,_,_,_),
	author(AuthorID,Author,_,_).

%Exercise 2
multi_genre_book(Title):-
	book(Title,_,_,_,Genres),
	length(Genres,L),
	L > 1.

%Exercise 3
not(X) :- X, !, fail.
not(_).

check([],_,[]).
check([H|T],List,[H|Rest]):-
	member(H,List),
	check(T,List,Rest).
check([H|T],List,Rest):-
	not(member(H,List)),
	check(T,List,Rest).
shared_genres(Title1,Title2,Common):-
	book(Title1,_,_,_,Genres1),
	book(Title2,_,_,_,Genres2),
	check(Genres1,Genres2,Common).

%Exercise 4
similarity(Title1,Title2,Sim):-
	book(Title1,_,_,_,Genres1),
	book(Title2,_,_,_,Genres2),
	shared_genres(Title1,Title2,Common),
	length(Genres1,L1),
	length(Genres2,L2),
	length(Common,Intersect),
	Union is L1 + L2 - Intersect,
	Sim is Intersect / Union.

%Exercise 6
circle_size(Person, Size) :-
    colect(Person, Person, 0, Size).

colect(Initial, Person, Acc, Size) :-
    gives_gift_to(Person, _, Initial),
    Size is Acc + 1, !.

colect(Initial, Person, Acc, Size) :-
    gives_gift_to(Person, _, Person2),
    Acc1 is Acc + 1,
    colect(Initial, Person2, Acc1, Size).

%Exercise 7
adding_people(X, Initial, [X]) :-
    gives_gift_to(X, _, Initial), !.

adding_people(X, Initial, [X|Rest]) :-
    gives_gift_to(X, _, X2),
    adding_people(X2, Initial, Rest).

normalize_cycle([H|T], Normalized) :-
    min_member(Min, [H|T]),
    rotate_until([H|T], Min, Normalized).

rotate_until([Min|T], Min, [Min|T]) :- !.
rotate_until([H|T], Min, Normalized) :-
    append(T, [H], Rotated),
    rotate_until(Rotated, Min, Normalized).

pair_sizes([], []).
pair_sizes([P|Ps], [(P,Size)|Rest]) :-
    circle_size(P, Size),
    pair_sizes(Ps, Rest).

largest_circle(People) :-
    setof(Person, X^gives_gift_to(Person, _, X), Persons),
    pair_sizes(Persons, Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, [(Initial,_)|_]),
    adding_people(Initial, Initial, RawCycle),
    normalize_cycle(RawCycle, People).

%Exercise 10
convert(0, []) :- !.
convert(Dec, [Bit|Rest]) :-
    Bit is Dec mod 2,
    Next is Dec // 2,
    convert(Next, Rest).

add_zeros(List, B, B, List) :- !.
add_zeros(List, A, B, Result) :-
    A < B,
    A1 is A + 1,
    add_zeros([0|List], A1, B, Result).

dec2bin(Dec, BinList, N) :-
    Max is 2 ** N,
    Dec < Max,
    Dec > 0,
    convert(Dec, BitsRev),
    reverse(BitsRev, Bits),
    length(Bits, L),
    ( L < N
    -> add_zeros(Bits, L, N, BinList)
    ;  BinList = Bits
    ).

%Exercuse 11
initialize(Dec,Bits,Padding,List):-
	M is Bits + Padding,
	dec2bin(Dec,Bin,M),
	reverse(Bin,Ipaded),
	M2 is M + 2,
	add_zeros(Ipaded,M,M2,Result),
	reverse(Result,List).

%Exercise 12
translate(1,'M').
translate(0,'.').

terminal(0,[]):-!,
	write('|'),nl.
terminal(0,Bits):- !,
	write('|'),
	terminal(8,Bits).
terminal(_,[]):- !,nl.
terminal(N,[B|Bits]):-
	N1 is N - 1,
	translate(B,Char),
	put_char(Char),
	terminal(N1,Bits).
print_generation(List):-
	write('|'),
	terminal(8, List).
