int: N = 10;

array [1..N] of var 1..N: x;
array [1..2] of var 1..N: y;
array [1..2] of var 1..N: c;
array [1..2] of var bool: b;

predicate count_op(array [int] of var int: x, var int: y, var int: c);

% should go through count_*_int_par and translate to CountConst and a comparison operator
constraint y[1] = N div 3;
constraint c[1] = N div 2;
constraint b[1] = count_op(x, y[1], c[1]);

% should translate to CountVar and a comparison operator
constraint b[2] = count_op(x, y[2], c[2]);

constraint b[1] \/ b[2];

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c), "\n",
    "b = ", show(b)];
