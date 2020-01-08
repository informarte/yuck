include "lex_lesseq.mzn";

int: N = 10;
int: M = N;

array[1..N] of var set of 0..1: x;
array[1..N] of var set of 0..1: y;

constraint lex_lesseq(x, y);
constraint forall (i in 1..M)(x[i] = {1});

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
