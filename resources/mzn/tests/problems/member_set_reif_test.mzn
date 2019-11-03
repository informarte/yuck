include "member.mzn";

array [1..10] of var set of 1..10: x;
var set of 1..10: y;
var set of 1..10: z;

constraint member(x, y) \/ member(x, z);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y), "\n",
        "z = ", show(z)];
