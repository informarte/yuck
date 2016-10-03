include "count_lt.mzn";

array [1..10] of var 1..10: x;

var 1..10: y;

constraint count_lt(x, y, 9);

solve satisfy;

output [show(x), "\n", show(y)];
