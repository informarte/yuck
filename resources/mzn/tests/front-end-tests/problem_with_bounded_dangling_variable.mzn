array [1..4] of var 0..9: x;

constraint x[1] + x[2] = x[3];

solve satisfy;

output ["x = ", show(x)];
