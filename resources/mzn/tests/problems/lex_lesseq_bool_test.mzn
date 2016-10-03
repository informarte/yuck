include "lex_lesseq.mzn";

int: n = 10;
int: m = 10;

array[1..10] of var bool: x;
array[1..10] of var bool: y;

constraint lex_lesseq(x, y);
constraint forall (i in 1..m)(x[i] = true);

solve satisfy;

output [show(x), show(y)];
