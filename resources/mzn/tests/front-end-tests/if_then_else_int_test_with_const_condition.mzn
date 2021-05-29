bool: c = true;
var int: x = if c then 1 else 2 endif;
var int: y = if not c then 1 else 2 endif;

constraint x = 1;
constraint y = 2;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
