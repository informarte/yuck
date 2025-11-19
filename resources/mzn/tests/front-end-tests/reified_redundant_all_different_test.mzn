include "all_different.mzn";

int: N = 3;

array [1..N] of var 1..N: x;
array [1..N] of var 1..N: y;

constraint (forall(i, j in index_set(x) where i < j) (x[i] != x[j]) /\ redundant_constraint(all_different(x))) \/
           (forall(i, j in index_set(x) where i < j) (y[i] != y[j]) /\ redundant_constraint(all_different(y)));

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
