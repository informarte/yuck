var bool: c;
var bool: x = if c then true else true endif;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x)];
