include "exactly.mzn";

array [1..10] of var 1..10: x;

constraint exactly(10, x, 1);

solve satisfy;

output ["x = ", show(x)];
