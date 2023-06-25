#!/bin/bash
#swipl -x "main(X)" run.pl
swipl -O -l run.pl -g main. -- "Learn_String_with_levenshtein" "stringopt"
