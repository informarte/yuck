include "lex_less.mzn";

int: N = 10;
int: M = 9;

array [1..N] of var 0..1: x;
array [1..N] of var 0..1: y;

constraint lex_less(x, y) \/ lex_less(y, x);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
