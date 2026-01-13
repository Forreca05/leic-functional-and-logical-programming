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
predZ:-
read(X),
X =.. [_|B],
length(B, N),
write(N), nl.

Lê um termo Prolog do utilizador

Conta quantos argumentos ele tem
*/

/*
Mantendo duplicados:
uniao([], L, L).
uniao([H|T], L, [H|R]) :-
    uniao(T, L, R).

intersecao([], _, []).
intersecao([H|T], L, [H|R]) :-
    member(H, L),
    intersecao(T, L, R).
intersecao([_|T], L, R) :-
    intersecao(T, L, R).

Sem duplicados:
uniao_set([], L, L).
uniao_set([H|T], L, R) :-
    member(H, L),
    uniao_set(T, L, R).
uniao_set([H|T], L, [H|R]) :-
    \+ member(H, L),
    uniao_set(T, L, R).

intersecao_set([], _, []).
intersecao_set([H|T], L, [H|R]) :-
    member(H, L),
    \+ member(H, R),
    intersecao_set(T, L, R).
intersecao_set([_|T], L, R) :-
    intersecao_set(T, L, R).
*/

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
%RECURSO 24/25

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
% TESTE 24/25

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
*/

/*
EPOCA ESPECIAL 24/25

:- dynamic by/3.
:- use_module(library(lists))
.
% by(Character, Movie, Actor)
by(jackRyan, theSumOfAllFears, benAffleck).
by(cathyMuller, theSumOfAllFears, bridgetMoynahan).
by(jackRyan, theHuntForRedOctober, alecBaldwin).
by(jackRyan, patriotGames, harrisonFord).
by(cathyMuller, patriotGames, anneArcher).
by(jackRyan, clearAndPresentDanger, harrisonFord).
by(cathyMuller, clearAndPresentDanger, anneArcher).
by(president, airForceOne, harrisonFord).
by(frasierCrane, cheers, kelseyGrammer).
by(frasierCrane, frasier, kelseyGrammer).
by(rachelGreen, friends, jenniferAniston).
by(monicaGeller, friends, courteneyCox).
by(phoebeBuffay, friends, lisaKudrow).
by(ursulaBuffay, friends, lisaKudrow).
by(joeyTribbiani, friends, mattLeBlanc).
by(joeyTribbiani, joey, mattLeBlanc).
by(alexGarrett, joey, andreaAnders).
by(stephenColbert, dailyShow, stephenColbert).
by(stephenColbert, theColbertReport, stephenColbert).
by(addisonMontgomery, privatePractice, kateWalsh).
by(addisonMontgomery, greysAnatomy, kateWalsh).
by(mattMurdock, daredevil, benAffleck).
by(elektraNatchios, daredevil, jenniferGarner).
by(elektraNatchios, elektra, jenniferGarner).
by(elektraNatchios, elektra, lauraWard).
by(sydneyBristow, alias, jenniferGarner).

%Exercise 11
plays_twins(Actor,Movie):-
	by(Character1,Movie,Actor),
	by(Character2,Movie,Actor),
	Character1 \= Character2.

%Exercise 12
not(X) :- X, !, fail.
not(_).

actor_movies(Actor,Movies):-
	actor_movies(Actor,[],Movies).
actor_movies(Actor,Acc,Movies):-
	by(_,Movie,Actor),
	not(member(Movie,Acc)),!,
	actor_movies(Actor,[Movie|Acc],Movies).
actor_movies(_Actor,L,L).

%Exercise 13
changeSelf:-
	retract(by(Actor,Movie,Actor)),
	assert(by(self,Movie,Actor)),
	fail.   %Prolog tenta outra vez
changeSelf.

%Exercise 14
playedBy(Character, List) :-
    setof(Actor, Movie^by(Character, Movie, Actor), Actors),
    findall(
        A-Ms,
        (
            member(A, Actors),
            findall(M, by(Character, M, A), Ms)
        ),
        List
    ).

%Exercise 15
max_count([N-_|Rest], Max) :-
    max_count(Rest, N, Max).

max_count([], Max, Max).
max_count([N-_|Rest], Acc, Max) :-
    N > Acc, !,
    max_count(Rest, N, Max).
max_count([_-_|Rest], Acc, Max) :-
    max_count(Rest, Acc, Max).

most_popular(Exclude, List, NMovies) :-
    setof(
        Actor,
        Movie^(by(_, Movie, Actor), not(member(Actor, Exclude))),
        Actors
    ),
    findall(
        Count-Actor,
        (
            member(Actor, Actors),
            setof(M, C^by(C, M, Actor), Movies),
            length(Movies, Count)
        ),
        Pairs
    ),
    max_count(Pairs, NMovies),
    findall(A, member(NMovies-A, Pairs), List).

% Exercise 16
connection_link(Actor1, Actor2, List):-
    connection_link(Actor1, Actor2, [Actor1], RList),
    reverse(RList, List).

connection_link(Actor1, Actor2, Tmp, [Actor2, Movie | Tmp]):-  
    by(_C1, Movie, Actor1),
    \+ member(Movie, Tmp),
    by(_C2, Movie, Actor2),
    Actor1 \= Actor2.

connection_link(Actor1, Actor2, Tmp, List):- 
    by(_C1, Movie, Actor1),
    \+ member(Movie, Tmp),
    by(_C2, Movie, Actor3),
    Actor1 \= Actor3,
    \+ member(Actor3, Tmp),
    connection_link(Actor3, Actor2, [Actor3, Movie | Tmp], List).

%Exercise 17
pretty_print([A,B,C|Rest]):-
	write(A), write(' worked in '), write(B), write(' with '), write(C),
	(Rest == []
	-> (write('.'),nl)
	; (write(','),nl)),pretty_print2(Rest).
pretty_print2([A,B|Rest]):-
	write(' who worked in '), write(A), write(' with '), write(B),
	(Rest == []
	-> (write('.'),nl)
	; (write(','),nl),pretty_print2(Rest)).

*/

/*
TESTE 22/23

:-use_module(library(lists)).

% dish(Name, Price, IngredientGrams).
dish(pizza, 2200, [cheese-300, tomato-350]).
dish(ratatouille, 2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread, 1060, [cheese-50, garlic-200]).

:- dynamic ingredient/2.

% ingredient(Name, CostPerGram).
ingredient(cheese, 4).
ingredient(tomato, 2).
ingredient(eggplant, 7).
ingredient(garlic, 6).

%Exercise 1
count_ingredients(Dish,Num):-
	dish(Dish,_,Ingredients),
	length(Ingredients,Num).

%Exercise 2
ingredient_amount_cost(Ingredient,Grams,TotalCost):-
	ingredient(Ingredient,Price),
	TotalCost is Price * Grams.

%Exercise 3
get_ingredients([],Price,Price):-!.
get_ingredients([Ingredient-Grams|T],Aux,Price):-
	ingredient_amount_cost(Ingredient,Grams,Cost),
	NewAux is Cost + Aux,
	get_ingredients(T,NewAux,Price).
	
dish_profit(Dish,Profit):-
	dish(Dish,Price,Ingredients),
	get_ingredients(Ingredients,0,Price2),
	Profit is Price - Price2.

%Exercise 4
update_unit_cost(Ingredient,New):-
	retract(ingredient(Ingredient,_Price)),
	assert(ingredient(Ingredient,New)).

%Exercise 5
most_expensive_dish(Dish, Price) :-
    dish(Dish, Price, _),
    \+ (
        dish(_, Price2, _),
        Price2 > Price
    ).

%Exercise 6- no recursion
consume_ingredient(Stocks,Ingredient,Grams,New):-
	member(Ingredient-Total,Stocks),
	Total >= Grams,
	NewTotal is Total - Grams,
	select(Ingredient-Total, Stocks, Ingredient-NewTotal, New).

%Exercise 7
count_dishes_with_ingredient(Ingredient,N):-
	count_dishes_with_ingredient_aux(Ingredient,[],0,N).
count_dishes_with_ingredient_aux(_, Visited, Aux, Aux) :-
	\+ ( dish(Name, _, _), \+ member(Name, Visited) ).
count_dishes_with_ingredient_aux(Ingredient,Visited,Aux,N):-
	dish(Name,_Price,Ingredients),
	member(Ingredient-_Total,Ingredients),
	\+ member(Name,Visited),
	Aux1 is Aux + 1,
	count_dishes_with_ingredient_aux(Ingredient,[Name|Visited],Aux1,N).
count_dishes_with_ingredient_aux(Ingredient,Visited,Aux,N):-
	dish(Name,_Price,Ingredients),
	\+ member(Ingredient-_,Ingredients),
	\+ member(Name,Visited),
	count_dishes_with_ingredient_aux(Ingredient,[Name|Visited],Aux,N).

%Exercise 8
formalize([],[]).
formalize([A-_|T],[A|Rest]):-
	formalize(T,Rest).
get_ingredientslist(Dish,List):-
	dish(Dish,_,IList),
	formalize(IList,List).
list_dishes(DishIngredients) :-
    setof(Dish-List,
          get_ingredientslist(Dish, List),
          DishIngredients).

%Exercise 9
most_lucrative_dishes(Dishes) :-
    findall(Profit-Dish, dish_profit(Dish, Profit), Pairs),
    keysort(Pairs, SortedAsc),
    reverse(SortedAsc, SortedDesc),
    extract_dishes(SortedDesc, Dishes).

extract_dishes([], []).
extract_dishes([_-Dish | T], [Dish | Rest]) :-
    extract_dishes(T, Rest).

% G1
edge(g1, br, o).
edge(g1, br, ni).
edge(g1, o, ni).
edge(g1, o, c).
edge(g1, o, h).
edge(g1, h, c).
edge(g1, h, n).
edge(g1, n, he).
edge(g1, c, he).

% G2
edge(g2, br, h).
edge(g2, br, ni).
edge(g2, h, ni).
edge(g2, h, o).
edge(g2, h, c).
edge(g2, o, c).
edge(g2, o, n).
edge(g2, n, he).
edge(g2, c, he).
edge(g2, cl, he).

%Exercise 21
common_edges(G1,G2,L):-
	setof(A-B,(edge(G1,A,B),edge(G2,A,B);edge(G1,A,B),edge(G2,B,A)),L).

%Exercise 22
% Arestas comuns entre G1 e G2
common_edges(G1, G2, L) :-
    setof(A-B,
        (edge(G1, A, B), edge(G2, A, B) ;
         edge(G1, A, B), edge(G2, B, A)),
        L), !.
common_edges(_, _, []).

% Encontra todos os vértices ligados a um vértice inicial
grow(_, [], Acc, Acc).
grow(Edges, [V|Rest], Acc, Final) :-
    findall(N,
        (member(V-X, Edges), N = X ; member(X-V, Edges), N = X),
        Neigh),
    subtract_list(Neigh, Acc, New),   % apenas remove duplicados
    append(Rest, New, Queue),
    append(Acc, New, Acc2),
    grow(Edges, Queue, Acc2, Final).

% subtract_list(List, Remove, Result) — versão simples
subtract_list([], _, []).
subtract_list([H|T], R, Out) :-
    member(H, R), !,
    subtract_list(T, R, Out).
subtract_list([H|T], R, [H|Out]) :-
    subtract_list(T, R, Out).

% Extrai componentes conexos
components([], _, []).
components([V|Vs], Edges, [C|Rest]) :-
    grow(Edges, [V], [V], C),
    subtract_list(Vs, C, Remaining),
    components(Remaining, Edges, Rest).

% Predicado principal
common_subgraphs(G1, G2, Subgraphs) :-
    common_edges(G1, G2, Edges),
    findall(X, (member(A-B, Edges), (X=A ; X=B)), Vs0),
    sort(Vs0, Vertices),
    components(Vertices, Edges, Subgraphs).
*/



/*
+ INPUT
- OUTPUT
? IN/OUT

GROUND- THERE ARE NO VARIABLES IN THE TERM
UNGROUND-

X / Y-FLOAT QUOTIENT
X // Y-INTEGER QUOTIENT,TRUNCATED TOWARDS 0
X div Y-INTEGER QUOTIENT(ROUNDED DOWN)
X rem Y-INTEGER REMAINDER: X-Y*(X//Y)
X mod Y-INTEGER REMAINDER: X-Y*(X div Y)

RED CUT INFLUENCES THE RESULT
GREEN CUT ONLY INCREASE EFFICIENCY

A = 4+2 UNIFIES A WITH THE TERM +(4,2), NOT THE VALUE 6

Diferença entre == e :=: em Prolog
Operador	Significado	Exemplo	Resultado
==	Igualdade sintática (mesmos termos, mesma estrutura, sem avaliação)	X = 2+2, X == 2+2	true
:=:	Igualdade aritmética (avalia ambos lados como expressões numéricas)	X = 2+2, X :=: 4	true

xfx só permite um único uso do operador entre dois termos. Ou seja, a borders b borders c não é permitido com xfx, pois o parser não sabe como agrupar.
xfy permite encadeamento à direita, como a borders b borders c, que será interpretado como a borders (b borders c).

Pensa que os operadores com precedência menor são mais gulosos — agarram os seus argumentos antes dos outros.
Ficam normalmente dentro dos parenteses e os outros e que ficam fora.
*/