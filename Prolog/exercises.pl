:- use_module(library(lists)).

% country(Country, Capital, Continent, Population)
country('Bolivia', 'Bogota', 'South America', 12.08).
country('Chile', 'Santiago', 'South America', 19.49).
country('Peru', 'Lima', 'South America', 33.72).

country('Mozambique', 'Maputo', 'Africa', 32.08).
country('South Africa', 'Cape Town', 'Africa', 59.39).
country('Lesotho', 'Maseru', 'Africa', 2.281).

:- dynamic borders/2.

% borders(Country, ListOfNeighbors)
borders('Bolivia',['Chile'-861, 'Peru'-4300]).
borders('Chile',['Peru'-169, 'Bolivia'-861]).
borders('Peru',['Chile'-169, 'Bolivia'-4300]).

borders('Mozambique',['South Africa'-496]).
borders('South Africa',['Mozambique'-496, 'Lesotho'-909]).
borders('Lesotho',['South Africa'-909]).

%Exercise 1
is_enclave(C):-
	borders(C,[_]).

%Exercise 2
long_border_neighbors(Min,C1,C2):-
	borders(C1,Neighbors),
	member(C2-Dist,Neighbors),
	Dist >= Min.

%Exercise 3
get_neighbors([],[]).
get_neighbors([H-_|T],[H|Rest]):-
	get_neighbors(T,Rest).
get_capitals([],[]).
get_capitals([H|T],[Capital|Rest]):-
	country(H,Capital,_,_),
	get_capitals(T,Rest).
neighbor_capitals(Country,Capitals2):-
	borders(Country,Neighbors),
	get_neighbors(Neighbors,List),
	get_capitals(List,Capitals),
	permutation(Capitals,Capitals2).

%Exercise 4
largest_population_of_continent(Continent,Country):-
	country(Country,_,Continent,Population),
	\+ (
		country(_Country2,_,Continent,Population2),
		Population2 > Population
	).

%Exercise 5
second_largest_population_of_continent(Cont, C) :-
    largest_population_of_continent(Cont, CMax),
    country(CMax,_,_,PopMax),
    country(C,_,Cont,Pop),
    Pop < PopMax,
    \+ ((
        country(_,_,Cont,Pop1),
        Pop1 < PopMax,
        Pop1 > Pop
    )).

%Exercise 6
similar_neighbors(C,N1,N2):-
	borders(C,Neighbors),
	member(N1-Border1,Neighbors),
	member(N2-Border2,Neighbors),
	Diff is Border1-Border2,
	abs(Diff) < 1000.

similar_neigbours(C,N1,N2) :-
    borders(C,L),
    append(_,[N1-Len1, N2-Len2 |_], L),
    (Len2 - Len1) < 1000.

%Exercise 7
are_borders_sorted(L) :-
    \+ ((
        append(_,[_-Len1, _-Len2 |_], L),
        Len2 < Len1
    )).

%Exercise 8
print_countries:-
	country(Country,C,Cont,_),
	write(C),write('. capital of '),write(Country),write(', in '), write(Cont),nl,
	fail.
print_countries.

%Exercise 9
get_countries(Countries) :-
    get_countries_aux([], Countries).

get_countries_aux(Acc, Countries) :-
    country(C,_,_,_),
    \+ member(C, Acc),
    !,
    get_countries_aux([C | Acc], Countries).
get_countries_aux(Acc, Acc).

%Exercise 10
big_country_continent_capitals(Min,Continent,Capitals):-
	bagof(Capital,(Country,Population)^(country(Country,Capital,Continent,Population),Population >= Min),Capitals).

%Exercise 11
most_populated_neighbours(C,L) :-
    borders(C,Ns),
    setof(Pop-N, (Len,Cap,Cont)^(member(N-Len,Ns), country(N,Cap,Cont,Pop)),Pairs),
    reverse(Pairs,DescPairs),
    findall(N, member(_-N, DescPairs), L).

%Exercise 12
trajectory(C1,C2,Traj) :-
    dfs([C1], C2, Traj).

dfs([C2|_], C2, [C2]).
dfs([Ca|T], C2, [Ca|Traj]) :-
    borders(Ca,L),
    member(Cb-_,L),
    \+ member(Cb,T),
    dfs([Cb, Ca | T], C2, Traj).
	
%Exercise 2.5
/*A: ✅ True. Incomplete lists (like [a,b|Tail]) allow constant-time concatenation because you can unify the tail with another list directly.

B: ⚠️ Partially true. While Prolog is based on FOL, not all constructs (like cuts or meta-predicates) translate cleanly.

C: ❌ False. Green cuts don’t always improve efficiency — they only avoid unnecessary backtracking. And not all cuts are green.

D: (Cut off) — but likely refers to the recursive nature of unification. Yes, Prolog’s unification can be implemented recursively, often using a stack or trail.*/