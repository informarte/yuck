bool: c = true;
var set of 1..10: u;
var set of 1..10: v;
var set of int: x = if c then u else v endif;
var set of int: y = if not c then u else v endif;

constraint x subset y;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "u = ", show(u), "\n",
    "v = ", show(v), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
