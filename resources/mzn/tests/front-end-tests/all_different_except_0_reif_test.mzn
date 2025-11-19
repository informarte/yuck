include "all_different_except_0.mzn";

int: N = 10;

array [1..N] of var 0..N: x;
array [1..N] of var 0..N: y;

constraint x[1] = x[N];
constraint y[1] = y[N];

constraint all_different_except_0(x) \/ all_different_except_0(y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
