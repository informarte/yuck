var -10..10: i;
var -10..10: j;
var -10..10: k;
array [1..10] of int: x = {7, 3, 2, 4, 9, 6, 1, 8, 5, 10};

constraint x[i] + x[j] = x[k];

solve satisfy;

output [
    "i = ", show(i), "\n",
    "j = ", show(j), "\n",
    "k = ", show(k), "\n",
    "x = ", show(x)];
