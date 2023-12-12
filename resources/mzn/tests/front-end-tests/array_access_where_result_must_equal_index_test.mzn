var 1..10: i;
array [1..10] of var 1..10: p;

constraint p[i] = i;

solve satisfy;

output [
    "p = ", show(p), "\n",
    "i = ", show(i)];
