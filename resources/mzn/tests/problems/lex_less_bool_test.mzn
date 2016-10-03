include "lex_less.mzn";

int: n = 10;
int: m = 9;

array[1..10] of var bool: x;
array[1..10] of var bool: y;

constraint lex_less(x, y);
constraint forall (i in 1..m)(x[i] = true);

solve satisfy;

output [show(x), show(y)];
