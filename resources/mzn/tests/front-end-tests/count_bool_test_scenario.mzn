int: N = 10;

array [1..N] of var bool: x;

predicate count_op(array [int] of var bool: x, var bool: y, var int: c);

% should go through count_*_bool_par and translate to CountConst and a comparison operator
constraint count_op(x, false, N div 2);

% should translate to CountVar and a comparison operator
var bool: y;
var 1..N: c;
constraint count_op(x, y, c);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
