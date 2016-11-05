include "count_gt.mzn";

array [1..10] of var 1..2: x;

var 1..2: y;

constraint count_gt(x, y, 1);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)];
