include "count_eq.mzn";

int: N = 10;

array [1..N] of var set of 1..N: x;
array [1..2] of var set of 1..N: y;
array [1..2] of var 1..N: c;

% should translate to CountConst and Contains
constraint y[1] = {i | i in 1..N where i mod 2 = 0};
constraint c[1] = count(x, y[1]);

% should translate to CountVar and Contains
constraint c[2] = count(x, y[2]);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
