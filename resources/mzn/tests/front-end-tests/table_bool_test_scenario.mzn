int: N = 21; % number of columns
int: M = 5; % number of rows

set of int: COLS = 1..N;
set of int: ROWS = 1..M;

array [ROWS, COLS] of bool: t = array2d(ROWS, COLS, [(i + j) mod 3 == 0 | i in ROWS, j in COLS]);

array [COLS] of var bool: x;

output ["x = ", show(x)];
