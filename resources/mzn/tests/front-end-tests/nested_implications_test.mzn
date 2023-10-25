var bool: c;
var bool: d;
var -10..10: x;

constraint c -> (x >= 0 /\ (d -> x <= 0));

solve satisfy;

output ["c = ", show(c), "\n",
        "d = ", show(d), "\n",
        "x = ", show(x)];
