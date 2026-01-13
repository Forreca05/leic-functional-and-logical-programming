female(grace).
female(claire).
female(gloria).
female(barb).
female(cameron).
female(haley).
female(dede).
female(lily).
female(pameron).
female(poppy).

male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(bo).
male(dylan).
male(alex).
male(luke).
male(rexford).
male(calhoun).
male(george).

parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(dede, mitchell).
parent(jay, claire).
parent(jay, mitchell).
parent(jay, joe).
parent(gloria, joe).
parent(gloria, manny).
parent(javier, manny).
parent(barb, cameron).
parent(barb, pameron).
parent(merle, cameron).
parent(merle, pameron).
parent(phil, haley).
parent(phil, alex).
parent(phil, luke).
parent(claire, haley).
parent(claire, alex).
parent(claire, luke).
parent(mitchell, lily).
parent(mitchell, rexford).
parent(cameron, lily).
parent(cameron, rexford).
parent(pameron, calhoun).
parent(bo, calhoun).
parent(dylan, george).
parent(dylan, poppy).
parent(haley, george).
parent(haley, poppy).

father(X,Y):- male(X), parent(X,Y).
mother(X,Y):- female(X), parent(X,Y).

grandfather(X,Y):- father(X,Z), parent(Z,Y).
grandmother(X,Y):- mother(X,Z), parent(Z,Y).

siblings(X,Y):-
    parent(Z,X), parent(S,X),
    parent(Z,Y), parent(S,Y),
    X \= Y.

halfsiblings(X,Y):-
    parent(Z,X),
    parent(Z,Y), 
    X \= Y,
    \+ siblings(X, Y).

cousins(X,Y):-
    parent(Z,X),
    parent(S,Y),
    siblings(Z, S),
    X \= Y.

uncle(X,Y):-
    parent(Z,Y),
    siblings(Z,X),
    male(X).
    