array [1..3] of var 0..9: x;
var int: y;

constraint x[1] + x[2] = x[3];

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)]
