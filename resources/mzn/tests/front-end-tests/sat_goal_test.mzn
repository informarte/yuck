include "alldifferent.mzn";
include "yuck.mzn";

int: N = 10;

array [1..N] of var 1..N: x;

constraint x[1] = x[N];

solve :: goal_hierarchy([sat_goal(alldifferent(x))]) satisfy;

output ["x = ", show(x)];
