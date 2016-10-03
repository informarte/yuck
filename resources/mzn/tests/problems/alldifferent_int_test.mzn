% Example taken from Global Constraint Catalog, 2nd Edition, Revision A

include "alldifferent.mzn";

array [1..6] of var int: x;

constraint x[1] in 2..4;
constraint x[2] in 2..3;
constraint x[3] in 1..6;
constraint x[4] in 2..5;
constraint x[5] in 2..3;
constraint x[6] in 1..6;

constraint alldifferent(x);

solve satisfy;

output [show(x)];
