include "alldifferent.mzn";

int: N = 100;

array [0..N] of var int: x;
constraint x[0] = 0;
constraint forall(i in 1..N)(x[i] in 1..N);

constraint alldifferent(x);
constraint sum(i in 0..9)(x[i]) == 777;

solve satisfy;

output ["x = ", show(x)];
