include "at_least.mzn";

array [1..10] of var 1..2: x;
constraint at_least(9, x, 2);

solve satisfy;

output [show(x)];
