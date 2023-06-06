# prolog_genetic_programming
genetic programming in prolog

we were inspired by the genetic programming example in python from [lowerkey/genetic_programming](https://github.com/lowerkey/genetic_programming)


## Example Task, Overfitting on a string

#### running manually

run swi-prolog, load the algorithm, and run it.
this runs the genetic programming to overfit on the string "Hello" and prints the genes for every iteration.

``` 
swipl
?- [load].
?- run_evolution("Learn String", "stringopt", _, EvolutionHistory, "stopcondition"), EvolutionHistory = [ LastEpoch | _ ], writeln(LastEpoch), !.
```
