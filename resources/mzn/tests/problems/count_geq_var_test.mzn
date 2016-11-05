include "count_geq.mzn";

array [1..10] of var 1..2: x;

var 1..2: y;

constraint count_geq(x, y, 1);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)];
