bird(robinho,    robin,   male,   [red, brown, white]).
bird(robina,     robin,   female, [brown, red, white]).
bird(ferrugem,   robin,   male,   [brown, gray]).

bird(arcoiris,   parrot,  male,   [red, blue, green, yellow]).
bird(verdeja,    parrot,  female, [green, yellow]).

bird(minerva,    owl,     female, [brown, white]).
bird(noctis,     owl,     male,   [gray, white]).
bird(sabia,      owl,     female, [beige, brown]).

male(Name):-
    bird(Name,_,male,_).


has_more_color_of(Name,Greater,Lesser):-
    bird(Name,_,_,Colors),
    append(_,[Greater|Rest],Colors),
    member(Lesser,Rest).


most_colorful(Species, Name, NColors) :-
    bird(Name, Species, _, Colors),
    length(Colors, NColors),
    \+ (
        bird(OtherName, Species, _, OtherColors),
        OtherName \= Name,
        length(OtherColors, OtherN),
        OtherN > NColors
    ).


unique_colors(Species, ListOfColors):-
    auxiliar(Species, [], ListOfColors).
auxiliar(Species, Acc, ListOfColors):-
    bird(_, Species, _, Colors),
    member(Color, Colors),
    \+ member(Color, Acc),
    !,
    auxiliar(Species, [Color|Acc], ListOfColors).
auxiliar(_Species, Acc, Acc).


is_color_permutation(Name1, Name2) :-
    bird(Name1, _, _, Colors1),
    bird(Name2, _, _, Colors2),
    Name1 \= Name2,
    sort(Colors1, Sorted),
    sort(Colors2, Sorted).


dif_n_colors(S, D):-
	findall(L, (bird(_,S, _,C), length(C,L)), List),
	max_list(List,M),
	min_list(List,Min),
	D is M - Min.
	
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, MaxT),
    (H > MaxT -> Max = H ; Max = MaxT).

min_list([X], X).
min_list([H|T], Min) :-
    min_list(T, MinT),
    (H < MinT -> Min = H ; Min = MinT).


% most_common_color_per_species(?Species, ?Color),
most_common_color_per_species(S,C):-
	findall(S1,bird(_,S1, _,_),Specs),
	sort(Specs, Specs1),
	member(S,Specs1),
	unique_colors(S,Cs),
	member(C,Cs),
	findall(N,(bird(N,S,_,C1),member(C,C1)),Birds),
	length(Birds,L1),
	\+((
		member(C2,Cs),
		findall(N,(bird(N,S,_,Cs2),member(C2,Cs2)),Birds2),
		length(Birds2,L2),
		L2>L1
	)).


:- use_module(library(lists)).

colorful_routes(Ni, Nf, L) :-
    bird(Ni, _Si, _Gi, _Ci),
    bird(Nf, _Sf, _Gf, _Cf),
    dfs_colorful(Ni, Nf, [Ni], RevPath),
    reverse(RevPath, L),
    length(L, Len),
    Len >= 5.

dfs_colorful(Current, Current, Path, Path).
dfs_colorful(Current, Goal, Visited, Path) :-
    bird(Current, _, _, [Main1|_]),
    bird(Next, _, _, [Main2|_]),
    Main1 \= Main2,
    \+ member(Next, Visited),
    dfs_colorful(Next, Goal, [Next|Visited], Path).


