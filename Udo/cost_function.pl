:- module(cost_function, [costfn/3,stopcondition/3,mapcost/3,levenshtein/3,quadratic_cost/3]).
:- use_module(core). 

%----------------------------------------------------------------------------------------------
% Target string - Helper function to test mapcost and stopcondition.
target_string("Hello").

%----------------------------------------------------------------------------------------------
% Usable Cost functions

% costfn for levenshtein distance
costfn("levenshtein", Gene, Cost) :-
    gene(Gene),
    target_string(T),
    levenshtein(Gene, T, Cost),
    !.

% costfn for quadratic cost
costfn("quadratic_cost", Gene, Cost) :-
    gene(Gene),
    target_string(T),
    quadratic_cost(Gene, T, Cost),
    !.

%----------------------------------------------------------------------------------------------
% stop condition

% stopcondition: when a member of the list has a cost of 0 or 0.00.
stopcondition("zero_cost", Costfn, Epoch) :-
    mapcost(Costfn, Epoch, Costs),
    (member(0, Costs); member(0.0,Costs), member(0.00, Costs)),
    !.

%----------------------------------------------------------------------------------------------
% Mapping Cost function

% mapcost: maps a cost function to a list of genes
mapcost(Costfn, [T], [CT]):-
    costfn(Costfn, T, CT),
    !.

mapcost(Costfn, [H|T],[Cost|CT]):-
    costfn(Costfn, H, Cost),
    mapcost(Costfn, T, CT),
    !.

%----------------------------------------------------------------------------------------------
% Cost function: Levenshtein distance

tail(String, Head, Tail) :- 
    sub_string(String, 1, _, 0, Tail), 
    sub_string(String, 0, 1, _, Head), 
    !.

% maybe look here https://occasionallycogent.com/levenshtein_distance/index.html
% levenshtein(Input, Target, Distance).
levenshtein(Input, Target, Distance):-
    string_length(Input, 0),
    string_length(Target,Distance),
    !.

levenshtein(Input, Target, Distance):-
    string_length(Target,0),
    string_length(Input,Distance),
    !.

levenshtein(Input, Target, Distance):-
    tail(Input, S, It),
    tail(Target, S, Tt),
    levenshtein(It,Tt,Distance),
    !.

levenshtein(Input, Target, Distance):-
    tail(Input,_,It),
    tail(Target,_,Tt),
    levenshtein(It,Target,D1),
    levenshtein(Input,Tt,D2),
    levenshtein(It,Tt,D3),
    min_list([D1,D2,D3], Olddistance),
    Distance is Olddistance+1,
    !.

%----------------------------------------------------------------------------------------------
% Cost function: Quadratic cost

quadratic_cost(Input, Target, Cost) :-
    string_chars(Target, TargetChars),
    string_chars(Input, InputChars),
    calculate_cost_helper(InputChars, TargetChars, 0, Cost).

% Base case: Input and Target is empty
calculate_cost_helper([], [], Acc, Acc).
% Base case: Target is empty
calculate_cost_helper(Code, [], Acc, Cost) :-
    length(Code, CodeLength),
    Cost is Acc + (2000 * CodeLength).
% Base case: Code is empty
calculate_cost_helper([], Target, Acc, Cost) :-
    length(Target, TargetLength),
    Cost is Acc + (2000 * TargetLength).
% Calculate the cost for each pair of characters
calculate_cost_helper([C1 | CodeRest], [T1 | TargetRest], Acc, Cost) :-
    char_code(C1, ASCII1),
    char_code(T1, ASCII2),
    Cost_tmp is sqrt(abs((ASCII2**2) - (ASCII1**2))),
    NewAcc is Acc + Cost_tmp,
    calculate_cost_helper(CodeRest, TargetRest, NewAcc, Cost).