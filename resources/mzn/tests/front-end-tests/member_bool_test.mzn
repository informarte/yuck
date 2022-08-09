include "member.mzn";

array [1..10] of var bool: x;
var bool: y;

constraint member([true, true], true);

constraint member(x, y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
