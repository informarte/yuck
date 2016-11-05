include "count_lt.mzn";

array [1..10] of var 1..10: x;

constraint count_lt(x, 1, 9);

solve satisfy;

output ["x = ", show(x)];
