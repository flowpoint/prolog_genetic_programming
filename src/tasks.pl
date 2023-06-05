:- module(tasks, [task/4, costfn/3, stopcondition/3, mapcost/3, levenshtein/3]).
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
    
task(
    "Learn String", 
    "accuracy",
    ["AAAA","BBBBB"],
    "zero_cost"
    ) :-
        true.

% maybe look here https://occasionallycogent.com/levenshtein_distance/index.html
levenshtein(Input, Target, Distance).

% additionally, maybe implement the other cost function from the python example


%levenshtein(A,B, Cost).
% per gene cost fn
costfn("accuracy", Gene, Cost) :-
    (Cost = 0, Gene = "Hello world");
    Cost = 1.

% costfn("abs", Gene, Cost) :-
%    atom_number(Gene, Cost).

stopcondition("zero_cost", Costfn, LastEpoch) :-
    mapcost(Costfn, LastEpoch, Costs),
    member(0, Costs).

mapcost(_, [], []).
mapcost(Costfn, [H|T], [Cost|CT]):-
    costfn(Costfn, H, Cost),
    mapcost(Costfn, T, CT).

% unused for now
kvs(Costfn, LastEpoch, Pairs) :-
    pairs_keys_values(
        Pairs,
        Costs,
        LastEpoch),
    mapcost(Costfn, LastEpoch, Costs).

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
qudratic_cost(Genes, Target, Population) :-
    string_chars(Target, TargetChars),
    calculate_cost_helper(Code, TargetChars, 0, Cost).

% Base case: Empty lists
calculate_cost_helper([], [], Acc, Acc).
% Base case: Code is empty
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
    Cost1 is sqrt(ASCII1**2 + ASCII2**2),
    NewAcc is Acc + Cost1,
    calculate_cost_helper(CodeRest, TargetRest, NewAcc, Cost).

