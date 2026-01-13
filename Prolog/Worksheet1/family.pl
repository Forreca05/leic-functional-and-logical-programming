:- module(family,[teaches/2, studentColleagues/2, female/1, male/1, siblings/2, mother/2, cousins/2, studentOfProfessor/2, attends/2, halfsiblings/2, parent/2, studentstop/1, teachersOfStudent/2, grandfother/2, professorcoleagues/2, uncle/2, grandmother/2, studentsOfProfessor/2, mutualStudent/2, father/2]).
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

grandfother(X,Y):- father(X,Z), parent(Z,Y).
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
    
teaches(algorithms, adalberto).
teaches(databases, bernardete).
teaches(compilers, capitolino).
teaches(statistics, diogenes).
teaches(networks, ermelinda).

attends(algorithms, alberto).
attends(algorithms, bruna).
attends(algorithms, cristina).
attends(algorithms, diogo).
attends(algorithms, eduarda).
attends(databases, antonio).
attends(databases, bruno).
attends(databases, cristina).
attends(databases, duarte).
attends(databases, eduardo).
attends(compilers, alberto).
attends(compilers, bernardo).
attends(compilers, clara).
attends(compilers, diana).
attends(compilers, eurico).
attends(statistics, antonio).
attends(statistics, bruna).
attends(statistics, claudio).
attends(statistics, duarte).
attends(statistics, eva).
attends(networks, alvaro).
attends(networks, beatriz).
attends(networks, claudio).
attends(networks, diana).
attends(networks, eduardo).

studentOfProfessor(X,Y):-
    teaches(Z,Y),
    attends(Z,X).

studentsOfProfessor(X,Y):-
    teaches(Z,X),
    attends(Z,Y).

teachersOfStudent(X,Y):-
    teaches(Z,Y),
    attends(Z,X).

mutualStudent(X,Y):-
    studentOfProfessor(X,Z),
    studentOfProfessor(Y,Z),
    X \= Y.

studentColleagues(X,Y):-
    attends(Z,X),
    attends(Z,Y),
    X \= Y.

professorcoleagues(X,Y):-
    teaches(Z,Y),
    teaches(Z,X),
    X \= Y.

studentstop(X):-
    attends(Y,X),
    attends(Z,X),
    Z @< Y.

pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

team(lamb, breitling).
team(besenyei, 'red bull').
team(chambliss, 'red bull').
team(maclean, mediterranean).
team(mangold, cobra).
team(jones, matador).
team(bonhomme, matador).

