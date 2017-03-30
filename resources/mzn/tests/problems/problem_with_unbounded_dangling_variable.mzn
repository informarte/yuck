array [0..2] of var 0..9: x;
var int: y;

constraint x[0] + x[1] = x[2];

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
