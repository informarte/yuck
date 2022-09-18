include "member.mzn";

int: N = 10;

array [1..N] of var bool: x;
array [1..2] of var bool: b;

% should translate to CountConst and Le
constraint b[1] = member(x, true);

% should translate to CountVar and Le
var bool: y;
constraint b[2] = member(x, y);

constraint b[1] \/ b[2];

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y), "\n",
        "b = ", show(b)];
