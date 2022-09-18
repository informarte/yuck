include "count_eq.mzn";

int: N = 10;

array [1..N] of var bool: x;
array [1..2] of var 1..N: c;

% should translate to CountConst and Contains
constraint c[1] = count(x, false);

% should translate to CountVar and Contains
var bool: y;
constraint c[2] = count(x, y);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
