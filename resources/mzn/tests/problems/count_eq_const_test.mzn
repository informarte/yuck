include "count_eq.mzn";

array [1..10] of var 1..10: x;

constraint count_eq(x, 1, 9);

solve satisfy;

output [show(x)];
