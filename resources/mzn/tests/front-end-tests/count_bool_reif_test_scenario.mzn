int: N = 10;

array [1..N] of var bool: x;
array [1..2] of var bool: b;

predicate count_op(array [int] of var bool: x, var bool: y, var int: c);

% should go through count_*_bool_par and translate to CountConst and a comparison operator
constraint b[1] = count_op(x, false, N div 2);

% should translate to CountVar and a comparison operator
var bool: y;
var 1..N: c;
constraint b[2] = count_op(x, y, c);

constraint b[1] \/ b[2];

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c), "\n",
    "b = ", show(b)];
