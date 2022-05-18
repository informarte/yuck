include "alldifferent.mzn";

int: N = 4;

array [1..N] of var set of 1..N: x;

constraint alldifferent(x);

constraint array_union(x) = 1..(N div 2);

solve satisfy;

output ["x = ", show(x)];
