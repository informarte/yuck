include "inverse.mzn";

int: n = 10;

array [1..n] of var int: f;
array [0..n-1] of var int: g;

constraint inverse(f, g);

solve satisfy;

output [
    "f = ", show(f), "\n",
    "g = ", show(g)];
