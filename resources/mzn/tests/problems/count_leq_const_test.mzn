include "count_leq.mzn";

array [1..10] of var 1..10: x;

constraint count_leq(x, 1, 9);

solve satisfy;

output [show(x)];
