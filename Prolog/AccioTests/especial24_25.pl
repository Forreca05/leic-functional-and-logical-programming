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
