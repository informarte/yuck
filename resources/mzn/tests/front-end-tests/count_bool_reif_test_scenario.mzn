int: N = 10;

array [1..N] of var bool: x;
array [1..2] of var bool: y;
array [1..2] of var 1..N: c;
array [1..2] of var bool: b;

predicate count_op(array [int] of var bool: x, var bool: y, var int: c);

% should go through count_*_bool_par and translate to CountConst and a comparison operator
constraint y[1] = false;
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
