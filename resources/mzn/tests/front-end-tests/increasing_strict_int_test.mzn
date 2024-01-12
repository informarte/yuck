include "strictly_increasing.mzn";

int: N = 100;

array [1..N] of var 1..(2 * N): x;

constraint strictly_increasing(x);
constraint sum(i in 1..N)(x[i]) > N * N;

solve satisfy;

output ["x = ", show(x)];
