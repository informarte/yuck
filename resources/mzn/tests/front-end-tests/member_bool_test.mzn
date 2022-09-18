include "member.mzn";

int: N = 10;

array [1..N] of var bool: x;

% should translate to CountConst and Le
constraint member(x, true);

% should translate to CountVar and Le
var bool: y;
constraint member(x, y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
