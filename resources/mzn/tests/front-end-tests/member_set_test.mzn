include "member.mzn";

int: N = 10;

array [1..N] of var set of 1..N: x;

% should translate to CountConst and Le
constraint member(x, {i | i in 1..N where i mod 2 = 0});

% should translate to CountVar and Le
var set of 1..N: y;
constraint member(x, y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
