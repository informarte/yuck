% Tests the ability of the compiler to create an appropriate neighbourhood
% for implicit solving of inverse in a situation where the latter is used to
% create disjoint pairs from a given set, like in elitserien, for example.

include "inverse.mzn";

int: N = 100;

array [1..N] of var 1..N: f;

constraint inverse(f, f);
constraint sum(i in 1..10)(f[i]) == 777;

solve satisfy;

output ["f = ", show(f)];
