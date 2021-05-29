var bool: c;
var set of int: x = if c then 1..3 else 1..5 endif;
var set of int: y = if not c then 1..3 else 1..5 endif;

constraint x subset y;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
