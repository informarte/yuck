include "member.mzn";

array [1..10] of var bool: x;
var bool: y;

constraint member(x, y) \/ member(x, not y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
