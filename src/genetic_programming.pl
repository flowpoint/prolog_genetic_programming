:- module(genetic_programming, [genetic_programming/3]).

task([TaskName, Costfn, Initializer, StopCondition]) :-
    % unique identifier to summarize the task
    TaskName = "Learn String",
    % cost function describes what the tasks goal is
    % by assigning a cost value to the "solutions"
    Costfn = _,
    % initializer describes the initial state the task should be started from
    % this is part of the task, so the optimizers can be agnostic to the tasks
    % additionally the task definition might contain realistically an inital 
    % state to be started from
    Initializer = _,
    % the stopcondition defines when a task is achieved
    StopCondition = _.

%optimizer(Selectionop, Crossoverop, Mutationop).
optimizer(_,_,_).

genetic_programming(Task, Optimizer, EvolutionHistory) :-
    task(Task),
    Optimizer = _,
    EvolutionHistory = ["Hello world"].

    
