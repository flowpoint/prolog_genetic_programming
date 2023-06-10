:- module(tasks, [task/4, costfn/3, stopcondition/3, mapcost/3, levenshtein/3, quadratic_cost/3]).
:- use_module(core).

% task([TaskName, Costfn, Initializer, StopCondition]) :-
%     % unique identifier to summarize the task
%     TaskName = _,
%     % cost function describes what the tasks goal is
%     % by assigning a cost value to the "solutions"
%     Costfn = _,
%     % initializer describes the initial state the task should be started from
%     % this is part of the task, so the optimizers can be agnostic to the tasks
%     % additionally the task definition might contain realistically an inital 
%     % state to be started from
%     Initializer = _,
%     % the stopcondition defines when a task is achieved
%     StopCondition = _.
%
% target_string("Hello world").
target_string("Hello").

task(
    "Learn String", 
    "levenshtein",
    [[
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
    ]],
    "zero_cost"
    ) :-
        true.

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


% additionally, maybe implement the other cost function from the python example


%levenshtein(A,B, Cost).
% per gene cost fn
costfn("accuracy", Gene, Cost) :-
    target_string(T),
    (Cost = 0, Gene = T);
    Cost = 1.

costfn("levenshtein", Gene, Cost) :-
    gene(Gene),
    target_string(T),
    levenshtein(Gene, T, Cost),
    !.

% costfn("abs", Gene, Cost) :-
%    atom_number(Gene, Cost).

mapcost(Costfn, [T], [CT]):-
    costfn(Costfn, T, CT),
    !.
mapcost(Costfn, [H|T], [Cost|CT]):-
    costfn(Costfn, H, Cost),
    mapcost(Costfn, T, CT),
    !.

stopcondition("zero_cost", Costfn, Epoch) :-
    mapcost(Costfn, Epoch, Costs),
    member(0, Costs),
    !.

% Generate a list with random genes.
% Parameter: Size, Char_Limit, TargetList
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
    random_between(33, 126, Code),
    char_code(Char, Code),
    atom_concat(Acc, Char, Acc1),
    Length1 is Length - 1,
    generate_random_code_helper(Length1, Acc1, Gene).


% Rework in Progress
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
