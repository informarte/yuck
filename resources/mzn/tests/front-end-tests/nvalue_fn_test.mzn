include "nvalue_fn.mzn";

array [1..3] of var 1..3: x;
array [1..3] of var 1..3: y;

% should translate to NumberOfDistinctValues and Contains
var 1..2: m = nvalue(x);

% should translate to NumberOfDistinctValues
var int: n = nvalue(y);

solve maximize m + n;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "n = ", show(n), "\n",
    "m = ", show(m)];
