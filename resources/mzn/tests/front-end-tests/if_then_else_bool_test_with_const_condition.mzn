bool: c = true;
var bool: x = if c then true else false endif;
var bool: y = if not c then true else false endif;

constraint x = true;
constraint y = false;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
