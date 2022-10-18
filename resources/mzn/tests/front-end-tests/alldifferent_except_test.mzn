include "alldifferent_except.mzn";

array [1..6] of var int: x;

constraint x[1] in 2..4;
constraint x[2] in 2..3;
constraint x[3] in 0..6;
constraint x[4] in 2..5;
constraint x[5] in 2..3;
constraint x[6] in 0..6;

constraint x[3] = x[6];

constraint alldifferent_except(x, {0, 6});

solve satisfy;

output ["x = ", show(x)];
