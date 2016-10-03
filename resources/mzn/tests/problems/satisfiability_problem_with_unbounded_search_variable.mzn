array [0..2] of var int: x;

constraint x[0] + x[1] = x[2];

solve satisfy;

output [show(x)];
