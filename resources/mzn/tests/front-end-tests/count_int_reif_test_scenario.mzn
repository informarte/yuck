int: N = 10;

array [1..N] of var 1..N: x;
array [1..2] of var bool: b;

predicate count_op(array [int] of var int: x, var int: y, var int: c);

% should go through count_*_int_par and translate to CountConst and a comparison operator
constraint b[1] = count_op(x, N div 3, N div 2);

% should translate to CountVar and a comparison operator
var 1..N: y;
var 1..N: c;
constraint b[2] = count_op(x, y, c);

constraint b[1] \/ b[2];

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c), "\n",
    "b = ", show(b)];
