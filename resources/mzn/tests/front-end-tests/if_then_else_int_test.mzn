var bool: c;
var int: x = if c then 1 else 2 endif;
var int: y = if not c then 1 else 2 endif;

constraint x + y = 3;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
