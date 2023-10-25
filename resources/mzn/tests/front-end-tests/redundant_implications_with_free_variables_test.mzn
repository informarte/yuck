include "all_different.mzn";

int: N = 3;

array[1..N] of var 1..N: x;

constraint all_different(x);

% inspired by mspsp.mzn
constraint redundant_constraint(
    forall
        (i, j in index_set(x) where i < j)
        (let {var bool: before} in ((before -> x[i] < x[j]) /\ (not(before) -> x[j] < x[i]))));

solve satisfy;

output [show(x)];
