include "strictly_increasing.mzn";

int: N = 10;

array [1..N] of var 1..(2 * N): x;

constraint strictly_increasing(x) \/ strictly_increasing(reverse(x));

solve satisfy;

output ["x = ", show(x)];
