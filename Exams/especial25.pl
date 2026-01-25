:- use_module(library(lists)).

:-dynamic by/3.
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

plays_twins(Actor,Movie):-
    by(Char1, Movie, Actor),
    by(Char2, Movie, Actor),
    Char1 \= Char2.

actor_movies(Actor, L):-
    actor_movies(Actor, [], L).
actor_movies(Actor, Acc, L):-
    by(_Char, Movie, Actor),
    \+ member(Movie, Acc), !,
    actor_movies(Actor, [Movie|Acc], L).
actor_movies(_Actor, Acc, Acc).

changeSelf:-
    retract(by(Actor,Movie,Actor)),
    assert(by(self,Movie,Actor)),
    fail.
changeSelf.


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

not(X) :- X, !, fail.
not(_).


max_count([], [], 0).
max_count([A-Movies | T], BestActors, Max) :-
    length(Movies, N),
    max_count(T, RestActors, RestMax),
    ( N > RestMax ->
        BestActors = [A],
        Max = N
    ; N == RestMax ->
        BestActors = [A | RestActors],
        Max = N
    ; otherwise ->
        BestActors = RestActors,
        Max = RestMax
    ).
most_popular(Exclude, List, N) :-
    findall(A-M,
            ( by(_,_,A),
              \+ member(A, Exclude),
              actor_movies(A, M)
            ),
            Pairs0),
    sort(Pairs0, Pairs),
    max_count(Pairs, List, N).


connection_link(Actor1,Actor2,[Actor1,Movie,Actor2]):-
    by(_,Movie,Actor1),
    by(_,Movie,Actor2).
connection_link(A1,A2,List):-
    connection_link(A1,A2,[A1],Lista),
    reverse(Lista,List).
connection_link(A1,A2,Tmp,List):-
    by(_,M1,A1),
    \+ member(M1,Tmp),
    (   by(_, M1, A2)
    ->  List = [A2,M1|Tmp]
    ;   by(_, M1, A3),
        A3 \= A1,
        \+ member(A3, Tmp),
        connection_link(A3, A2, [A3, M1 | Tmp], List)
    ).

pretty_print([A, B, C|T]):-
    write(A), write(' worked in '), write(B), write(' with '), write(C),
    pretty_print2(T).
pretty_print2([]):-
    write('.'), nl.
pretty_print2([B, C|T]):-
    write(','), nl, write(' who worked in '), write(B), write(' with '), write(C),
    pretty_print2(T).
