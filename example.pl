:- use_module(src/tasks).
:- use_module(src/genetic_programming).

% adding tasks is easy by dynamically adding a task definition here

target_fun(X, Y) :-
    Y is X*X+1.

:- asserta(
(
tasks:task("Learn_polynomial_function", "distance_from_fun", Initializer, "zero_cost") :-
    Initializer = [["target_fun(X,Y) :- Y = X."]],
    true
)
).


:- dynamic pred_fun/2.

%:- asserta(pred_fun(A, B) :- true).

evalu(Str, Inp, Res) :-
    term_string(T, Str),
    asserta(T),
    apply(pred_fun, [Inp, Res]),
    !.

% evalu("pred_fun(A,B) :- A is B+1.", 1, R).
% catch(evalu("pred_fun(A,B):- B is A+1., 1, P), _, P=100).

eval_catch(Gene, Inp, Pred) :-
    catch(evalu(Gene, Inp, P), _, P=100),
    Pred is P,
    !.

:- asserta(
(
tasks:costfn("distance_from_fun", Gene, Cost) :- 
    Input = 1,
    %number_string(Input, Gene),
    target_fun(Input, Res),
    eval_catch(Gene, Input, Pred),
    Cost is abs(Res-Pred)
)
).


% :- Gene="write(1", tasks:costfn("distance_from_fun", Gene, C), write(C).
% :- Gene="1", tasks:costfn("distance_from_fun", Gene, C), write(C).
% :- Gene="target_fun(X,Y):- Y=X*X+1.", tasks:costfn("distance_from_fun", Gene, C), write(C).

% Gene="target_fun(X,Y):- Y=X*X+1.", evalu(Gene, 1, Res).



%:- genetic_programming:genetic_programming("Learn_polynomial_function", A, B).


