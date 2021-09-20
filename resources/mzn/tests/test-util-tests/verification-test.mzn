var 0..10: x;
var 0..10: y;

constraint x + y < 10;

output [
    "x = ", show(x), "\n",
    "y = ", show(y)];
