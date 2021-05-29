var bool: c;
var int: x = if c then 1 else 0 endif;
var 1..5: y;

constraint x + 3 * y >= 10;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
