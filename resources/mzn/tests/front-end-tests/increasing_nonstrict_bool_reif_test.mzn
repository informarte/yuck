include "increasing.mzn";

int: N = 10;

array [1..N] of var bool: x;

constraint increasing(x) \/ increasing(reverse(x));

solve satisfy;

output ["x = ", show(x)];
