:- module(tasks, [task/4, costfn/3, stopcondition/3, mapcost/3]).
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
