var {1, 3, 7, 9}: i;
var {2, 4, 6, 8, 10}: j;
var {2, 3, 4, 6, 7, 8, 9}: k;
array [1..10] of var 1..10: x;

constraint x[i] + x[j] = x[k];

solve satisfy;

output [
    "i = ", show(i), "\n",
    "j = ", show(j), "\n",
    "k = ", show(k), "\n",
    "x = ", show(x)];
