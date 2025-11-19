include "all_different.mzn";

int: N = 3;

array [1..N] of var 1..N: x;

constraint forall(i, j in index_set(x) where i < j) (x[i] != x[j]);

constraint redundant_constraint(all_different(x));

solve satisfy;

output ["x = ", show(x)];
