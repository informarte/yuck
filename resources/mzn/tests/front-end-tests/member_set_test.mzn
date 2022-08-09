include "member.mzn";

array [1..10] of var set of 1..10: x;
var set of 1..10: y;

constraint member([{1}, {1}], {1});

constraint member(x, y);

solve satisfy;

output ["x = ", show(x), "\n",
        "y = ", show(y)];
