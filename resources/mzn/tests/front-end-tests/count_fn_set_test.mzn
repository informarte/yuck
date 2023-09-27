include "count_eq.mzn";

int: N = 12;

array [1..N] of var set of 1..N: x;
array [1..4] of var set of 1..N: y;
array [1..4] of var 0..N: c;

% should translate to CountConst and Contains
constraint y[1] = {i | i in 1..N where i mod 2 = 0};
constraint c[1] > 0;
constraint c[1] = count(x, y[1]);

% should translate to CountVar and Contains
constraint c[2] > 0;
constraint c[2] = count(x, y[2]);

% should translate to CountConst
constraint y[3] = {i | i in 1..N where i mod 4 = 0};
constraint c[3] = count(x, y[3]);

% should translate to CountVar
constraint c[4] = count(x, y[4]);

% something to chew on
constraint sum(i in 1..4)(c[i]) = N;

solve satisfy;

output [
    "x = ", show(x), "\n",
    "y = ", show(y), "\n",
    "c = ", show(c)];
