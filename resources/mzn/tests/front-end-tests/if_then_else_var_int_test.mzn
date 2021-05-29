var bool: c;
var 1..10: u;
var 1..10: v;
var int: x = if c then u else v endif;
var int: y = if not c then u else v endif;

constraint x < y;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "u = ", show(u), "\n",
    "v = ", show(v), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
