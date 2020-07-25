var -10..10: i;
var -10..10: j;
var -10..10: k;
array [1..10] of var 1..10: x;

constraint x[i] + x[j] = x[k];

solve satisfy;

output [
    "i = ", show(i), "\n",
    "j = ", show(j), "\n",
    "k = ", show(k), "\n",
    "x = ", show(x)];
