% Example taken from Global Constraint Catalog, 2nd Edition, Revision A

include "nvalue.mzn";

array [1..3] of var int: x;
var 0..3: n;

constraint x[1] in 1..2;
constraint x[2] in 2..4;
constraint x[3] in 1..2;

constraint nvalue(n, x);

solve maximize n;

output [
    "x = ", show(x), "\n",
    "n = ", show(n)];
