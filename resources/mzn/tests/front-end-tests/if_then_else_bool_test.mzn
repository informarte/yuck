var bool: c;
var bool: x = if c then true else false endif;
var bool: y = if not c then true else false endif;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
