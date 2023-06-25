/** <module> Tasks Module
This module contains the tasks to be solved by the optimizers.
This module also contains the cost functions and stop conditions for the optimizers.
@author flowpoint,shinpanse
@license GPL-3.0
*/
:- module(tasks, [task/4, costfn/3, stopcondition/3, mapcost/3, levenshtein/3, quadratic_cost/3]).
:- use_module(core).
:- use_module(core).


% Target String
target_string("Hello").

% Task 1: Learn a String with levenshtein distance
task("Learn_String_with_levenshtein", "levenshtein",[["Levenshtein Go","Es ist warm","Eis am Stiel","","","","",""]],"zero_cost") :-
    true.

% Task 2: Learn a String with quadratic cost
task("Learn_String_with_quadratic", "quadratic_cost",[["Quadratic Go","Sommerfest","Polizei auf Segway","","","","",""]],"zero_cost") :-
    true.

% Task 3: Learn a String with Generate Population
task("Learn_String_with_Generate_Pop", "quadratic_cost", Initializer, "zero_cost") :-
    generate_population(8,10,Population),
    Initializer = [Population],
    true.


/** Cost Functions
 * @param Unique identifier of the cost function
 * @param Gene The gene to be evaluated
 * @return Cost The cost of the gene
*/
:- table costfn/3.

costfn("accuracy", Gene, Cost) :-
    target_string(T),
    (Cost = 0, Gene = T);
    Cost = 1.


costfn("levenshtein", Gene, Cost) :-
    gene(Gene),
    target_string(T),
    levenshtein(Gene, T, Cost),
    !.


costfn("quadratic_cost", Gene, Cost) :-
    gene(Gene),
    target_string(T),
    quadratic_cost(Gene, T, Cost),
    !.

/** Stop Conditions
 * Stops the optimization process if the cost of the gene is 0
 * @param Unique identifier of the stop condition
 * @param Costfn The cost function to be used
 * @param Epoch The current epoch
*/
stopcondition("zero_cost", Costfn, Epoch) :-
    mapcost(Costfn, Epoch, Costs),
    (member(0, Costs); member(0.00, Costs)),
    !.

/** Map Cost
 * Maps the cost function to a list of genes
 * @param Costfn The cost function to be used
 * @param Genes The list of genes to be evaluated
 * @return Costs The list of costs
*/
mapcost(Costfn, [T], [CT]):-
    costfn(Costfn, T, CT),
    !.
mapcost(Costfn, [H|T], [Cost|CT]):-
    costfn(Costfn, H, Cost),
    mapcost(Costfn, T, CT),
    !.

/** Levenshtein Distance
 * Calculates the levenshtein distance between two strings
 * @param Input The input string
 * @param Target The target string
 * @return Distance The levenshtein distance between the input and the target string
*/
tail(String, Head, Tail) :- 
    sub_string(String, 1, _, 0, Tail), 
    sub_string(String, 0, 1, _, Head), 
    !.

:- table levenshtein/3.

% maybe look here https://occasionallycogent.com/levenshtein_distance/index.html
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

/** Quadratic Cost
 * Calculates the quadratic cost between two strings
 * @param Input The input string
 * @param Target The target string
 * @return Cost The quadratic cost between the input and the target string
*/
:- table quadratic_cost/3.
quadratic_cost(Input, Target, Cost) :-
    string_chars(Target, TargetChars),
    string_chars(Input, InputChars),
    calculate_cost_helper(InputChars, TargetChars, 0, Cost),
    !.

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

/** Generate Random String List
 * Generates a list of random strings
 * @param Size The size of the list
 * @param Char_Limit The maximum length of the strings
 * @return The list of random strings as Population
*/
generate_population(Size, Char_Limit, Population) :-
    generate_genes_list_helper(Size, Char_Limit, Population).

generate_genes_list_helper(0, _, []).

generate_genes_list_helper(Size, Char_Limit, [Gene | Rest]) :-
    Size > 0,
    generate_random_gene(Char_Limit, Gene),
    Size1 is Size - 1,
    generate_genes_list_helper(Size1, Char_Limit, Rest).

generate_random_gene(Char_Limit, Gene) :-
    random_between(1, Char_Limit, Length),
    generate_random_code(Length, Gene).

generate_random_code(Length, Gene) :-
    generate_random_code_helper(Length, '', Gene).

generate_random_code_helper(0, Gene, Gene).
generate_random_code_helper(Length, Acc, Gene) :-
    Length > 0,
    random_between(65, 90, Code),
    char_code(Char, Code),
    atom_concat(Acc, Char, Acc1),
    Length1 is Length - 1,
    generate_random_code_helper(Length1, Acc1, Gene).
