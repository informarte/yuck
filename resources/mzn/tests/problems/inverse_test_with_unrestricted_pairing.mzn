% Tests the ability of the system to set up and use implicit solving for
% inverse in cases where every x in f can be paired with every y in g.

include "inverse.mzn";

int: N = 100;

array [1..N] of var 0..(N - 1): f;
array [0..(N - 1)] of var 1..N: g;

constraint inverse(f, g);
constraint sum(i in 0..9)(g[i]) == 777;

solve satisfy;

output [
    "f = ", show(f), "\n",
    "g = ", show(g)];
