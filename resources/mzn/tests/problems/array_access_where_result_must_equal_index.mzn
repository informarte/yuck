var 1..10: x;
array [1..10] of var 1..10: p;

constraint p[x] = x;

solve satisfy;

output [
    "p = ", show(p), "\n",
    "x = ", show(x), "\n"];
