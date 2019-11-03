include "lex_lesseq.mzn";

int: N = 10;
int: M = 10;

array [1..N] of var bool: x;
array [1..N] of var bool: y;

constraint lex_lesseq(x, y) \/ lex_lesseq(y, x);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
