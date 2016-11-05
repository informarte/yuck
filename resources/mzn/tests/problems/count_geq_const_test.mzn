include "count_geq.mzn";

array [1..10] of var 1..2: x;

constraint count_geq(x, 2, 1);

solve satisfy;

output ["x = ", show(x)];
