# prolog_genetic_programming
genetic programming in prolog

we were inspired by the genetic programming example in python from [lowerkey/genetic_programming](https://github.com/lowerkey/genetic_programming)


## Builtin Task, Overfitting on a string

#### running from cli

for ease of use, you can use the shell script run.sh on linux
or look into run.sh to see how to invoke swipl

```
./run.sh
```

#### running manually

run swi-prolog, load the algorithm, and run it.
this runs the genetic programming to overfit on the string "Hello" and prints the genes for every iteration.

``` 
swipl
?- [load].
?- genetic_programming:genetic_programming("Learn_String_with_levenshtein", "stringopt", EvolutionHistory), EvolutionHistory = [ LastEpoch | _ ], writeln(LastEpoch), !.
?- genetic_programming:genetic_programming("Learn_String_with_quadratic", "stringopt", EvolutionHistory), EvolutionHistory = [ LastEpoch | _ ], writeln(LastEpoch), !.
```


## Adding your own task, example.pl

You can add and redefine operations in your own external scripts through dynamic predicates.

In example.pl we define a new task and change the required operations.

you can run example.pl like follows:

```
?- [example].
```
