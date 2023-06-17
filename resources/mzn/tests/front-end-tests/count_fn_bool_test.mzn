include "count_eq.mzn";

int: N = 10;

array [1..N] of var bool: x;
array [1..2] of var bool: y;
array [1..2] of var 1..N: c;

% should translate to CountConst and Contains
constraint y[1] = false;
constraint c[1] = count(x, y[1]);

% should translate to CountVar and Contains
constraint c[2] = count(x, y[2]);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
