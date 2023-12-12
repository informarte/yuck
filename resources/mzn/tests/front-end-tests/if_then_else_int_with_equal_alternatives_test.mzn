var bool: c;
var int: x = if c then 1 else 1 endif;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x)];
