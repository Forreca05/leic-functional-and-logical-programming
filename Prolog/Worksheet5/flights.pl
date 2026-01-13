%flight(origin, destination, company, code, hour, duration)
flight(porto, lisbon, tap, tp1949, 1615, 60).
flight(lisbon, madrid, tap, tp1018, 1805, 75).
flight(lisbon, paris, tap, tp440, 1810, 150).
flight(lisbon, london, tap, tp1366, 1955, 165).
flight(london, lisbon, tap, tp1361, 1630, 160).
flight(porto, madrid, iberia, ib3095, 1640, 80).
flight(madrid, porto, iberia, ib3094, 1545, 80).
flight(madrid, lisbon, iberia, ib3106, 1945, 80).
flight(madrid, paris, iberia, ib3444, 1640, 125).
flight(madrid, london, iberia, ib3166, 1550, 145).
flight(london, madrid, iberia, ib3163, 1030, 140).
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165).

uniao_set([], L, L).
uniao_set([H|T], L, R) :-
    member(H, L),
    uniao_set(T, L, R).
uniao_set([H|T], L, [H|R]) :-
    \+ member(H, L),
    uniao_set(T, L, R).

get_all_nodes(List):-
	setof(Origin,A^B^C^D^E^flight(Origin,A,B,C,D,E),Origins),
	setof(Destination,A^B^C^D^E^flight(A,Destination,B,C,D,E),Destinations),
	uniao_set(Origins,Destinations,List).

company_cities(Company, Cities) :-
    setof(City,
          A^B^C^D^E^(
              flight(A, B, Company, C, D, E),
              (City = A ; City = B)
          ),
          Cities).

company_city_count(Company, Count) :-
    company_cities(Company, Cities),
    length(Cities, Count).

max_city_count(Max) :-
    setof(Count,
          C^company_city_count(C, Count),
          Counts),
    last(Counts, Max).

most_diversified(Company) :-
    max_city_count(Max),
    company_city_count(Company, Max).


find_flights(Origin, Destination, Flights) :-
    dfs(Origin, Destination, [Origin], Flights).
dfs(Destination, Destination, _, []).
dfs(Current, Destination, Visited, [Code|Rest]) :-
    flight(Current, Next, _, Code, _, _),
    \+ member(Next, Visited),
    dfs(Next, Destination, [Next|Visited], Rest).

find_all_flights(Origin, Destination, ListOfFlights) :-
    findall(Flights,
            find_flights(Origin, Destination, Flights),
            ListOfFlights).

	