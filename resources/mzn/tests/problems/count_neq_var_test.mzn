include "count_neq.mzn";

array [1..1] of var 1..2: x;

var 1..2: y;

constraint count_neq(x, y, 1);

solve satisfy;

output [show(x), "\n", show(y)];
