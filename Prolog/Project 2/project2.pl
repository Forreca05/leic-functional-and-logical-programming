%%%%%%%%%%%%%% PROJETO 2 %%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).

colors([green, yellow, blue, orange, white, black]).

consecutive(X,Y,Board) :-
	append(_,[X,Y|_], Board).

spaced(X,Y,Board):-
	append(_,[X,_,Y|_],Board);
	append([X],[_,_,_,Y,_],Board);
	append([_,X],[_,_,_,Y],Board).

acrossed(X,Y,Board):-
	append([X],[_,_,Y,_,_],Board);
	append([X],[_,_,_,Y,_],Board);
	append([X],[_,_,_,_,Y],Board);
	append([_,X],[_,Y,_,_],Board);
	append([_,X],[_,_,_,Y],Board);
	append([_,X],[_,_,Y,_],Board).

same(X,Y,Board):-
	append([X,Y],_,Board);
	append([_,_,_],[X,Y,_],Board);
	append([_,_,_],[X,_,Y],Board);
	append([_,_,_],[_,X,Y],Board).

next_to(X,X,_).
next_to(X,Y,[A,B,C,D,E,F]) :-
	consecutive(X,Y,[A,B,C,D,E,F]).
next_to(X,Y,[A,B,C,D,E,F]) :-
	consecutive(Y,X,[A,B,C,D,E,F]).

anywhere(X,[A,B,C,D,E,F]):-
	append(_,[X|_],[A,B,C,D,E,F]).

one_space(X,X,_).
one_space(X,Y,[A,B,C,D,E,F]):-
	spaced(X,Y,[A,B,C,D,E,F]).
one_space(X,Y,[A,B,C,D,E,F]):-
	spaced(Y,X,[A,B,C,D,E,F]). 

across(X,X,_).
across(X,Y,[A,B,C,D,E,F]):-
	acrossed(X,Y,[A,B,C,D,E,F]).
across(X,Y,[A,B,C,D,E,F]):-
	acrossed(Y,X,[A,B,C,D,E,F]).

same_edge(X,X,_).
same_edge(X,Y,[A,B,C,D,E,F]):-
	same(X,Y,[A,B,C,D,E,F]).
same_edge(X,Y,[A,B,C,D,E,F]):-
	same(Y,X,[A,B,C,D,E,F]).

position(_, [], _) :- fail.
position(X, [H|_], Board) :-
    nth1(H, Board, X), !.
position(X, [_|L], Board) :-
    position(X, L, Board).

board(Board) :-
    colors(C),
    permutation(C, Board).

check([], _).
check([H|T], Board):-
    call(H, Board),
    check(T, Board).

solve(Constraints, Board):-
    board(Board),
    check(Constraints, Board).

count_satisfied([], _, 0).
count_satisfied([H|T], Board, N) :-
    (   call(H, Board)
    ->  count_satisfied(T, Board, N1),
        N is N1 + 1
    ;   count_satisfied(T, Board, N)
    ).

board_score(Constraints, Board, Score) :-
    count_satisfied(Constraints, Board, Score).

best_score(Constraints, BestScore) :-
    findall(
        S,
        ( board(Board),
          board_score(Constraints, Board, S)
        ),
        Scores
    ),
    max_member(MaxSatisfied, Scores),
    length(Constraints, Total),
    BestScore is MaxSatisfied - Total.   %or just MaxSatisfied - 6.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- use_module(library(lists)).

% asserta(FactOrRule) Adiciona um facto ou cláusula no início da base de conhecimento.
% retract(FactOrRule) Remove da base de conhecimento um facto ou uma cláusula que unifica com o argumento.
/*
\+ G significa:

    “G não é demonstrável.”  
    Ou seja, falha se G for verdadeiro, e tem sucesso se G falhar.

É literalmente negação por falha (negation as failure).
*/

% member(X, [X|_]).
% member(X, [_|T]) :-
%    member(X, T).

% permutation(ListaOriginal, ListaPermutada)

% length(?List, ?Size)
/* length( [ ], 0 ).
   length( [_|T], L ):-
		length(T, L1),
		L is L1+1.*/

% member(?Elem, ?List)
/* member( X, [X|_] ).
   member( X, [_|T] ):-
		member(X, T).*/

% memberchk(?Elem, ?List)
/* memberchk( X, [X|_] ).
   memberchk( X, [Y|T] ):-
		X \= Y,
		memberchk(X, T).*/

% append(?L1, ?L2, ?L3)
/* append( [ ], L2, L2 ).
   append( [H|T], L2, [H|T3] ):-
		append(T, L2, T3).*/

% sort(+List, -SortedList)  sort(6, @=<, Classes, Sorted)
% keysort(+PairList, -SortedList)

% nth0(?Pos, ?List, ?Elem) / nth1(?Pos, ?List, ?Elem)
% nth0(?Pos, ?List, ?Elem, ?Rest) / nth1(?Pos, ?List, ?Elem, ?Rest)

% select(?X, ?XList, ?Y, ?YList) finds an occurrence of X in XList, replaces it with Y, and produces YList
% delete(+List, +ToDel, -R)
% delete(+List, +ToDel, +Count, -R) Deletes Count occurrences of ToDel in List, result R
% last(?Init, ?Last, ?List) Last element of List and the rest in Init

% segment(?List, ?Segment) succeed when Segment is a contiguous subsequence of List.
% sublist(+List, ?Part, ?Before, ?Length, ?After) extract a contiguous Part of List with Length size and Before/After pre/suffix

% append(+ListOfLists, -List) concate of Haskell
% reverse(?List, ?Reversed)
% rotate_list(+Amount, ?List, ?Rotated) 

% transpose(?Matrix, ?Transposed) converts rows into columns (and vice-versa)
% remove_dups(+List, ?PrunedList) 
% permutation(?List, ?Permutation) List permutations, with backtracking

% sumlist(+ListOfNumbers, ?Sum)
% max_member(?Max, +List)
% min_member(?Min, +List)
% max_member(:Comp, ?Max, +List) Comp is a comparison predicate of arity 2 used to compare elements
% min_member(:Comp, ?Min, +List)

% maplist(:Pred, +L) / maplist(:Pr, +L1, ?L2) / maplist(:Pr, +L1, ?L2, ?L3) Applies predicate to each element / map / zipWith
% map_product(:Pred, +Xs, +Ys, ?List) Cartesian product

% scanlist(:Pred, +Xs, ?Start, ?Final) foldl
% cumlist(:Pred, +Xs, ?Start, ?List) Similar to accumulate in python

% some(:Pred, +List) any
% include(:P, +X, ?L) / include(:P, +X, +Y, ?L) / include(:P, +X, +Y, +Z, ?L) filter / P(x, y) succeeds, L ⊆ X / P(x, y, z) succeeds, L ⊆ X
% exclude(:P, +X, ?L) / exclude(:P, +X,+Y, ?L) / exclude(:P, +X,+Y,+Z, ?L) 
% group(:Pred, +List, ?Front, ?Back) Group until predicate fails, splitting the list at that point


% read/1 reads a term (by default, from the standard input)
% • Input needs to end with a period (spans multiple lines)
% • If a compound term is being read, input must match term being read
% • Use unnamed variables (_X)

% write/1 writes a term
% nl/0 prints a new line

% get_char obtains a single character
% get_code obtains the ASCII code of a single character
% put_char prints a single character
% put_code prints a single character given its ASCII code
% char_code(?Atom, ?Code) allows converting between character and corresponding ASCII code
% get_byte and put_byte read and write binary data
% peek_char, peek_code and peek_byte obtain a single character /code / byte without consuming it from the input stream
% format prints terms with specified formatting options  format("~s", [T]),
% skip_line skips any input until the end of the line

% see/1 opens a file for reading
%• The file is used for reading instead of the standard input
% seen/0 closes the file that was opened for reading
% tell/1 opens a file for writing
%• The file is used for writing instead of the standard output
% told/0 closes the file that was opened for writing

% repeat always succeeds
%• Can be used to repeat some portion of code until it succeeds
% between(+Lower, +Upper, ?Number) can be used both to test and generate integers between given bounds
%• Necessary to include the between library

% atom_length('Q ', L).
% sub_atom(Day, 0, 1, _, Char) sub_atom(Atom, Start, Length, After, SubAtom)
% atom_number(Char, NDay)

% findall(?Term, :Goal, -List). findall finds all solutions, including repetitions if present
% bagof has similar behavior, but results are grouped by variables appearing in Goal but not in the search Term, and returns fails if there are no results
% We can direct bagof to ignore additional variables in Goal by using existential quantifiers: Var^Goal
% setof has similar behavior to bagof, but results are ordered and without repetitions

/*
DFS
connects_dfs(S, F):-
	connects_dfs(S, F, [S]).

connects_dfs(F, F, _Path).
connects_dfs(S, F, T):-
	connected(S, N),
	not( memberchk(N, T) ),
	connects_dfs(N, F, [N|T]).
	
BFS
connects_bfs(S, F):-
	connects_bfs([S], F, []).
connects_bfs([F|_], F, _V).
connects_bfs([S|R], F, V):-
	findall(
		N,
		( connected(S, N),
		not(memberchk(N, V)),
		not(memberchk(N, [S|R]))),
		L),
	append(R, L, NR),
	connects_bfs(NR, F, [S|V]).*/

/* 
binary_tree(null).
binary_tree( node(Value, Left, Right) ):-
binary_tree(Left),
binary_tree(Right).

tree_member(Val, node(Val, _L, _R) ).
tree_member(Val, node(V, L, _R) ):-
	[Val < V,] tree_member(Val, L).
tree_member(Val, node(V, _L, R) ):-
	[Val > V,] tree_member(Val, R).
	
tree_list( null, [] ).
tree_list( node(Val, L, R), List ):-
	tree_list(L, Left),
	tree_list(R, Right),
	append(Left, [Val|Right], List).

tree_is_ordered(Tree):-
	tree_list(Tree, List),
	sort(List, List).
	
tree_insert( null, V, node(V, null, null) ).
tree_insert( node(V, L, R), V, node(V, L, R) ).
tree_insert( node(V, L, R), Val, node(V, NL, R) ):-
	Val < V, tree_insert( L, Val, NL).
tree_insert( node(V, L, R), Val, node(V, L, NR) ):-
	Val > V, tree_insert( R, Val, NR).

tree_height( null, 0).
tree_height( node(Val, L, R), H):-
	tree_height(L, HL),
	tree_height(R, HR),
	H is 1 + max(HL, HR).

tree_is_balanced( null ).
tree_is_balanced( node(Val, L, R) ):-
	tree_is_balanced(L),
	tree_is_balanced(R),
	tree_height(L, HL),
	tree_height(R, HR),
	abs(HL-HR) =< 1.*/

/* 
• integer(A) A is an integer
• float(A) A is a floating point number
• number(A) A is a number (integer or float)
• atom(A) A is an atom
• atomic(A) A is an atom or a number
• compound(A) A is a compound term
• var(A) A is a variable (it is not instantiated)
• nonvar(A) A is an atom, a number or a compound term
• ground(A) A is nonvar, and all substructures are nonvar*/

% functor(+Term, ?Name, ?Arity) or functor(?Term, +Name, +Arity)
%• If Term is instantiated, returns the name and arity of the term
% • If Term is not instantiated, creates a new term with given name and arity

% arg(+Index, +Term, ?Arg)

% +Term =.. ?[Name | Args] or ?Term =.. +[Name | Args]
% • Given a term, returns a list with the name and arguments of the term
% • Given a proper list, creates a new term with name and arguments as specified by the contents of the list

% call/1 can be used with up to 255 arguments, in which case the first term is extended with the remaining arguments

/*
map(_, []).
map(P, [H|T]):-
G =.. [P, H],
G,
map(P, T).*/

/*
The op/3 predicate can be used to specify new operators
op(+Precedence, +Type, +Name).
• Precedence is a number between 1 and 1200(lower numbers have precedence)
• Type defines the type and associativity of the operator
	• Prefix – fx or fy
	• Postfix – xf or yf
	• Infix – xfx, xfy or yfx
• f defines the position of the operator
• x and y represent the operands
• x means non-associative
• y means side-associative*/



% statistics/0 prints statistics related to memory usage, execution time, garbage collection and others (counting from session start)
% statistics(?Keyword, ?Value) obtains values (or lists of values) for several available statistics



%%%%%%%%%%%%%%%%% EXAMES %%%%%%%%%%%%%%%%%
/* 
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
*/

/*
*/