var bool: c;
var set of int: x = if c then 1..10 else 1..10 endif;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x)];
