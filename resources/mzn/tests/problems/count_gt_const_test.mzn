include "count_gt.mzn";

array [1..10] of var 1..2: x;

constraint count_gt(x, 2, 1);

solve satisfy;

output ["x = ", show(x)];
