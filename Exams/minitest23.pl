:-use_module(library(lists)).

% dish(Name, Price, IngredientGrams).
dish(pizza, 2200, [cheese-300, tomato-350]).
dish(ratatouille, 2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread, 1600, [cheese-50, garlic-200]).

:- dynamic ingredient/2.
% ingredient(Name, CostPerGram).
ingredient(cheese, 4).
ingredient(tomato, 2).
ingredient(eggplant, 7).
ingredient(garlic, 6).

count_ingredients(D,N):-
    dish(D,_Price,Ing),
    length(Ing,N).

ingredient_amount_cost(Ing, Grams, Tot):-
    ingredient(Ing,Cost),
    Tot is Cost * Grams.

dish_profit(Dish,Profit):-
    dish(Dish,Price,Ing),
    aux(Ing,PriceR,0),
    Profit is Price - PriceR.

aux([],Acc,Acc).
aux([A-B|T],Profit,Acc):-
    ingredient_amount_cost(A,B,Tot),
    Total is Acc + Tot,
    aux(T,Profit,Total).

update_unit_cost(Ing,New):-
    retractall(ingredient(Ing,_)),
    assert(ingredient(Ing,New)).

most_expensive_dish(Dish,Price):-
    dish(Dish,Price,_),
    \+ (dish(Dish2,Price2,_),
	Dish2 \= Dish,
	Price2 > Price).

consume_ingredient(Stocks, Ing, Gram, New):-
    consume_ingredients(Stocks,Ing,Gram,New).
consume_ingredients([], _, _, []).
consume_ingredients([Ing-Qty | T], Ing, Gram, [Ing-NewQty | T]) :-
    Qty >= Gram,
    NewQty is Qty - Gram.
consume_ingredients([A-B | T], Ing, Gram, [A-B | T2]) :-
    A \= Ing,
    consume_ingredient(T, Ing, Gram, T2).

/*count_dishes_with_ingredient(Ing, N) :-
    count_aux(Ing, 0, N).

count_aux(Ing, Acc, N) :-
    dish(_, _, Ingredients),
    member(Ing-_, Ingredients),
    Acc1 is Acc + 1,
    fail.   % força backtracking para continuar a procurar

count_aux(_, N, N).*/

get_ingredients([],Acc,Acc).
get_ingredients([A-_|T],Acc,Lista):-
    get_ingredients(T,[A|Acc],Lista).
list_dishes(L):-
    bagof(D-Fim, I^P^(dish(D,P,I),get_ingredients(I,[],Fim)),L).

most_lucrative_dishes(L):-
    setof(D, P^Price^I^(dish(D,Price,I),
		  dish_profit(D,P)), L2),
    reverse(L2,L).
