var bool: c = true;
var bool: u;
var bool: v;
var bool: x = if c then u else v endif;
var bool: y = if not c then u else v endif;

constraint x != y;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "u = ", show(u), "\n",
    "v = ", show(v), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
