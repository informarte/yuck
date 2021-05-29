var bool: c;
var bool: d;
var bool: e;
var bool: x = if c then d else e endif;
var bool: y = if not c then d else e endif;

solve satisfy;

output [
    "c = ", show(c), "\n",
    "d = ", show(d), "\n",
    "e = ", show(e), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
