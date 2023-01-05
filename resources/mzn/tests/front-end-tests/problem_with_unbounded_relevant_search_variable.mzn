include "count_eq.mzn";

array [1..2] of var int: x;

constraint count_eq(x, 5, 1);

solve satisfy;

output ["x = ", show(x)];
