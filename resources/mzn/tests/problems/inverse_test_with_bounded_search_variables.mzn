include "inverse.mzn";

int: N = 10;

array [1..N] of var 0..(N - 1): f;
array [0..(N - 1)] of var 1..N: g;

constraint inverse(f, g);

solve satisfy;

output [
    "f = ", show(f), "\n",
    "g = ", show(g)];
