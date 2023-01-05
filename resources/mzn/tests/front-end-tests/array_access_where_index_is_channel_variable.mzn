var -10..10: i;
var -10..10: j;
var 1..10: k;
var 1..10: y;
array [1..10] of var 1..10: x;

constraint k = i + j;
constraint y = x[k];

solve satisfy;

output [
    "i = ", show(i), "\n",
    "j = ", show(j), "\n",
    "k = ", show(k), "\n",
    "x = ", show(x), "\n",
    "y = ", show(y)];
