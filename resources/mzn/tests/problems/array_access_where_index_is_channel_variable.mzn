var -10..10: i;
var -10..10: j;
var 1..10: k;
var 1..10: x;
array [1..10] of var 1..10: p;

constraint k = i + j;
constraint x = p[k];

solve satisfy;

output [
    "i = ", show(i), "\n",
    "j = ", show(j), "\n",
    "k = ", show(k), "\n",
    "p = ", show(p), "\n",
    "x = ", show(x), "\n"];
