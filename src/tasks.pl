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
%
% target_string("Hello world").
target_string("Hello").
    
task(
    "Learn String", 
    "levenshtein",
    [[
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell",
    "Hell"
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
