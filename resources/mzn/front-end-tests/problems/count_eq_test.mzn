include "count_eq.mzn";

array [1..10] of var 1..3: x;
array [1..4] of var 1..10: y;
array [1..4] of var 1..10: c;

constraint y[1] = 1 /\ c[1] = 2 /\ count_eq(x, y[1], c[1]);
constraint             c[2] = 3 /\ count_eq(x, y[2], c[2]);
constraint y[3] = 3             /\ count_eq(x, y[3], c[3]);
constraint                         count_eq(x, y[4], c[4]);

constraint sum(c) = 10;

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
