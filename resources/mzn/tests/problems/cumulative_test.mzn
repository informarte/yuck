% Example taken from Global Constraint Catalog, 2nd Edition, Revision A

include "cumulative.mzn";

array [1..4] of var int: o;
array [1..4] of var int: d;
array [1..4] of var 1..9: e;
array [1..4] of var int: h;

constraint o[1] in 1..5 /\ d[1] in 4..4 /\ h[1] in 2..6;
constraint o[2] in 2..7 /\ d[2] in 6..6 /\ h[2] in 3..3;
constraint o[3] in 3..6 /\ d[3] in 3..6 /\ h[3] in 1..2;
constraint o[4] in 1..8 /\ d[4] in 2..3 /\ h[4] in 3..4;

constraint forall (i in 1..4)(e[i] = o[i] + d[i]);

constraint cumulative(o, d, h, 5);

solve satisfy;

output [
    "o = ", show(o), "\n",
    "d = ", show(d), "\n",
    "h = ", show(h)];
