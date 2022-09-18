include "member.mzn";

int: N = 10;

array [1..N] of var 1..N: x;

% should translate to CountConst and Le
constraint member(x, N div 2);

% should translate to CountVar and Le
var 1..N: y;
constraint member(x, y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
