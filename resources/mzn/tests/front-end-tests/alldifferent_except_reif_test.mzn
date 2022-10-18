include "alldifferent_except.mzn";

int: N = 10;

array [1..N] of var 0..N: x;
array [1..N] of var 0..N: y;

constraint x[1] = x[N];
constraint y[1] = y[N];

constraint alldifferent_except(x, {0, N}) \/ alldifferent_except(y, {0, N});

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
