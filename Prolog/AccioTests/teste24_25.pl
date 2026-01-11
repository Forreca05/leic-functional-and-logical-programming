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


similarity(Title1,Title2,Sim):-
	book(Title1,_,_,_,Genres1),
	book(Title2,_,_,_,Genres2),
	shared_genres(Title1,Title2,Common),
	length(Genres1,L1),
	length(Genres2,L2),
	length(Common,Intersect),
	Union is L1 + L2 - Intersect,
	Sim is Intersect / Union.
