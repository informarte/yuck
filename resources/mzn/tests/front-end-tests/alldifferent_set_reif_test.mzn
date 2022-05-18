include "alldifferent.mzn";

int: N = 10;

array [1..N] of var set of 1..N: x;
array [1..N] of var set of 1..N: y;

constraint not alldifferent(x) \/ not alldifferent(y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
