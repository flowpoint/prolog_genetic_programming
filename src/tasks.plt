:- begin_tests(tasks).
:- use_module(core).
:- use_module(tasks).

test(mapcost) :-
    mapcost("accuracy", ["Hello world","2"], [0,1]).

:- end_tests(tasks).

