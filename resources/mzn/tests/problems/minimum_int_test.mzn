array [1..10] of var 1..10: x;

constraint min(x) = 10;

solve satisfy;

output [show(x)];
