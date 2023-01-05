var int: x;
var bool: r :: is_defined_var;

constraint r <-> x = 5;

solve satisfy;

output ["x = ", show(x), "\n",
        "r = ", show(r)];
