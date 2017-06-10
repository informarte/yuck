include "alldifferent.mzn";

int: N = 100;

array [0..N] of var int: x;
constraint x[0] = 0;
constraint forall(i in 1..N)(x[i] in 1..(2 * N) /\ x[i] != i);

constraint alldifferent(x);
constraint sum(i in 0..9)(x[i]) = 9 * N;

solve satisfy;

output ["x = ", show(x)];
