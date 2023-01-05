var 1..10: x;
var bool: r;

constraint r <-> x = 5;

solve satisfy;

output ["x = ", show(x), "\n",
        "r = ", show(r)];
