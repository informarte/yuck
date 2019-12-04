include "table.mzn";

int: N = 5; % number of columns
int: M = 0; % number of rows

set of int: COLS = 1..N;
set of int: ROWS = 1..M;

array [COLS] of var 0..10: x;

constraint table(x, array2d(ROWS, COLS, []));

solve satisfy;

output ["x = ", show(x)];
