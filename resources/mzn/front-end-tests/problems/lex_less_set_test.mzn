include "lex_less.mzn";

int: N = 10;
int: M = N - 1;

array[1..N] of var set of 0..1: x;
array[1..N] of var set of 0..1: y;

constraint lex_less(x, y);
constraint forall (i in 1..M)(x[i] = {1});

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
