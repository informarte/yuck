bool: c = true;
var set of int: x = if c then 1..3 else 5..7 endif;
var set of int: y = if not c then 1..3 else 5..7 endif;

constraint 1 in x;
constraint 5 in y;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
