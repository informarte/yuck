include "lex_less.mzn";

int: N = 10;
int: M = 9;

array [1..N] of var bool: x;
array [1..N] of var bool: y;

constraint lex_less(x, y);
constraint forall(i in 1..M)(x[i] = true);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
