include "alldifferent.mzn";
include "yuck.mzn";

int: N = 10;

array [1..N] of var 1..N: x;

constraint x[1] = x[N];

% To check that the compiler can cope with sat goals covering only a subset of the problem's
% search variables, we introduce y and constrain it.
var 1..N: y;
constraint abs(x[1] - y) = 2;

solve :: goal_hierarchy([sat_goal(alldifferent(x))]) satisfy;

output ["x = ", show(x)];
