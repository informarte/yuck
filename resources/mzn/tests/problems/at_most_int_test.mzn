include "at_most.mzn";

array [1..10] of var 1..2: x;
constraint at_most(1, x, 2);

solve satisfy;

output [show(x)];
