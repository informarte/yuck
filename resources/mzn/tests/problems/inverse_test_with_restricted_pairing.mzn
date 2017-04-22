% Tests the ability of the system to set up and use implicit solving for
% inverse in cases where NOT every x in f can be paired with every y in g.

include "inverse.mzn";

int: N = 50;

array [1..N] of var 0..N-1: f;
array [0..N-1] of var 1..N: g;

constraint inverse(f, g);
constraint f[1] = N - 1;
constraint forall(i in 1..N)(f[i] != i - 1);
constraint forall(j in 0..N-1)(g[j] != j + 1);
constraint g[N - 1] = 1;
constraint sum(i in 0..9)(g[i]) == 222;

solve satisfy;

output [
    "f = ", show(f), "\n",
    "g = ", show(g)];
