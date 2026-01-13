:- consult('family.pl').
:- consult('classes.pl').
:- consult('flights.pl').

children(Person, Children):-
    findall(Child, parent(Person,Child), Children).

children_of([], []).
children_of([H|T], [H-C|Rest]):-
    children(H,C),
    children_of(T,Rest).
    
family(F):-
    findall(P, (male(P) ; female(P)), L),
    sort(L, F).

couple(X-Y) :-
    parent(X, C),
    parent(Y, C),
    X @< Y.

couples(L):-
    setof(X-Y,couple(X-Y),L).




same_day(C1,C2):-
	class(C1,_,X,_,_),
	class(C2,_,X,_,_),
	C1 @=< C2.

daily_courses(D,L):-
	findall(Course,class(Course,_,D,_,_),L).

short_classes(L):-
	findall(Course-Day/Time,(class(Course,_,Day,_,Time),Time<2),L).

course_classes(Course,Classes):-
    findall(Day/Time-Type,class(Course,Type,Day,Time,_),Classes).

courses(L):-
    setof(Course, ClassType^Day^Time^Duration^class(Course, ClassType, Day, Time, Duration), L).

translate_day('1 Mon', 'Mon').
translate_day('2 Tue', 'Tue').
translate_day('3 Wed', 'Wed').
translate_day('4 Thu', 'Thu').
translate_day('5 Fri', 'Fri').

print_classes([]).
print_classes([Day-Time-Course-Type-Duration | Rest]) :-
    translate_day(Day, DisplayDay),
    format("~w ~w ~w at ~w (~w hours)~n",
           [Course, Type, DisplayDay, Time, Duration]),
    print_classes(Rest).

schedule :-
    setof(
        Day-Time-Course-Type-Duration,
        class(Course, Type, Day, Time, Duration),
        Sorted
    ),
    print_classes(Sorted).

find_class :-
    write('Day (e.g., \'1 Mon\'): '),
    read(Day),
    write('Time (e.g., 10.5): '),
    read(Time),
    (
        class(Course, Type, Day, Start, Duration),
        Time >= Start,
        Time =< Start + Duration
    ->
        format("Class: ~w (~w), starts at ~w, duration ~w hours.~n",
               [Course, Type, Start, Duration])
    ;
        write('No class is taking place at that time.'), nl
    ).
