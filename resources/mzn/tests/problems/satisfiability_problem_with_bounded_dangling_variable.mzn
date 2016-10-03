array [0..3] of var 0..9: x;

constraint x[0] + x[1] = x[2];

solve satisfy;

output [show(x)];
