include "count_neq.mzn";

array [1..1] of var 1..2: x;

constraint count_neq(x, 1, 1);

solve satisfy;

output [show(x)];
