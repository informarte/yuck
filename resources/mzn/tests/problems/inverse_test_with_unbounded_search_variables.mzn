include "inverse.mzn";

int: N = 10;

array [1..N] of var int: f;
array [0..(N - 1)] of var int: g;

constraint inverse(f, g);

solve satisfy;

output [
    "f = ", show(f), "\n",
    "g = ", show(g)];
