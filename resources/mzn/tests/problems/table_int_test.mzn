include "table.mzn";

int: N = 5; % number of columns
int: M = 21; % number of rows

set of int: COLS = 1..N;
set of int: ROWS = 1..M;

array [ROWS, COLS] of int: t = array2d(ROWS, COLS, [i + j - 1 | i in ROWS, j in COLS]);

% big domains exercise the presolver
array [COLS] of var -10000..10000: x;

constraint table(x, t);

solve satisfy;

output ["x = ", show(x)];
