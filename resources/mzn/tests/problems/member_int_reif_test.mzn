include "member.mzn";

array [1..10] of var 1..10: x;
var 1..10: y;

constraint member(x, y) \/ member(x, y div 2);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
