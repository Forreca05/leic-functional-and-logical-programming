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

read_book(bernardete,'The Firm').
read_book(bernardete,'The Client').
read_book(clarice,'The Firm').
read_book(clarice,'Carrie').
read_book(deirdre,'The Firm').
read_book(deirdre,'Next').

%Exercise 1
book_genre(Title, Genre):-
	book(Title,_,_,_,Genres),
	member(Genre,Genres).

%Exercise 2
author_wrote_book_at_age(Author,Title,Age):-
	author(AuthorID,Author,BYear,_),
	book(Title,AuthorID,RYear,_,_),
	Age is RYear - BYear.

%Exercise 3
youngest_author(Author):-
	author_wrote_book_at_age(Author,_,Age1),
	\+ (author_wrote_book_at_age(_,_,Age2),Age2<Age1). %Não existe nenhum outro autor que tenha escrito um livro com idade menor que Age1.

%Exercise 4
genres(Title):-
	book(Title,_,__,_,Genres),
	member(Genre,Genres),
	write(Genre),nl,fail.
genres(_).

%Exercise 5
getArgs(_,[],[]).
getArgs(Term,[H|T],[Arg|Rest]):-
	arg(H,Term,Arg),
	getArgs(Term,T,Rest).
filterArgs(Term,Indexes,NewTerm):-
	getArgs(Term,Indexes,Args),
	Term =.. [Func|_],
	NewTerm =.. [Func|Args].

%Exercise 6
diverse_books(Books):-
	findall(Title,(book(Title,_,_,_,Genres),length(Genres,Len),
					\+ ((book(_,_,_,_,Genres2), length(Genres2,Len2), Len2>Len)) ), Books).

%Exercise 7
country_authors(Country,Authors):-
	bagof(Name,(AuthorID^Year^author(AuthorID,Name,Year,Country)),Authors).

%Exercise 8
popular(Title):-
	setof(Reader,_B^read_book(Reader,_B),Readers),
	length(Readers,L),
	setof(Reader,read_book(Reader,Title),HaveRead),
	length(HaveRead,N),
	N / L >= 0.75.

%Exercise 12
:- op(730,xfx,wrote).
:- op(720,xfx,at).
Author wrote Book at Age:-
	author_wrote_book_at_age(Author,Book,Age).
% como at tem maior prioridade, agrup primeiro (Author wrote Book) at Age 

%Exercise 13
rotate(List,Position,Rotations,NewList):-
	nth1(Position,List,Torotate,Rest),
	rotate_list(Rotations,Torotate,Rotated),
	nth1(Position,NewList,Rotated,Rest).

%Exercise 14
equals([],_,0,_):-!.
equals([H|T],Code,N,B):-
	nth1(1,H,Elem1),
	nth1(B,Code,Elem2),
	Elem1 == Elem2,
	N1 is N - 1,
	B1 is B + 1,
	equals(T,Code,N1,B1).
matches(List,Code):-
	length(List,L1),
	length(Code,L2),
	L1 == L2,
	equals(List,Code,L1,1).

matches([], []).
matches([[H|_]|T], [H|TC]) :- 
	matches(T, TC).