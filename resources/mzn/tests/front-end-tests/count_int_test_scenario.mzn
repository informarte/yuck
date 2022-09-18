int: N = 10;

array [1..N] of var 1..N: x;

predicate count_op(array [int] of var int: x, var int: y, var int: c);

% should go through count_*_int_par and translate to CountConst and a comparison operator
constraint count_op(x, N div 3, N div 2);

% should translate to CountVar and a comparison operator
var 1..N: y;
var 1..N: c;
constraint count_op(x, y, c);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
