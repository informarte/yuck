include "inverse.mzn";

int: n = 10;

array [1..n] of var 0..n-1: f;
array [0..n-1] of var 1..n: g;

constraint f[3] = f[1] + f[2];
constraint g[n-1] = g[n-2] + g[n-3];

constraint inverse(f, g);

solve satisfy;

output [show(f), "\n", show(g)];
