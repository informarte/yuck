include "increasing.mzn";

int: N = 10;

array [1..N] of var 1..(N div 2): x;

constraint increasing(x) \/ increasing(reverse(x));

solve satisfy;

output ["x = ", show(x)];
