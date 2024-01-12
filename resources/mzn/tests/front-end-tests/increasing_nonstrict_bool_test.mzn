include "increasing.mzn";

int: N = 100;

array [1..N] of var bool: x;

constraint increasing(x);
constraint sum(i in 1..N)(x[i]) = N div 2;

solve satisfy;

output ["x = ", show(x)];
