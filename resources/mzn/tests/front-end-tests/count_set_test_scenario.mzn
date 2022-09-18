int: N = 10;

array [1..N] of var set of 1..N: x;

predicate count_op(array [int] of var set of int: x, var set of int: y, var int: c);

% should go through count_*_set_par and translate to CountConst and a comparison operator
constraint count_op(x, {i | i in 1..N where i mod 2 = 0}, N div 2);

% should translate to CountVar and a comparison operator
var set of 1..N: y;
var 1..N: c;
constraint count_op(x, y, c);

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
