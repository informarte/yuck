include "alldifferent.mzn";

int: N = 100;

array [1..N] of var int: x;
constraint forall(i in 1..N)(x[i] in 1..(2 * N) /\ x[i] != i);

constraint alldifferent(x);
constraint sum(i in 1..10)(x[i]) = 9 * N;

solve satisfy;

output ["x = ", show(x)];
