% Based on an example from the Global Constraint Catalog, 2nd Edition, Revision A

% The example consists of six rectangles and has only one solution, see p. 1040.

include "diffn_nonstrict.mzn";

int: N = 6;

array [1..N] of var 1..7: x;
array [1..N] of var 1..7: y;
array [1..N] of var 1..7: w;
array [1..N] of var 1..7: h;

% rectangles to be placed
constraint w[1] = 2 /\ h[1] = 5;
constraint w[2] = 5 /\ h[2] = 1;
constraint w[3] = 2 /\ h[3] = 4;
constraint w[4] = 4 /\ h[4] = 2;
constraint w[5] = 3 /\ h[5] = 3;
constraint x[6] = 1 /\ y[6] = 1 /\ w[6] = 3 /\ h[6] = 3;

% bounding box
constraint forall(i in 1..N)(x[i] + w[i] <= 8 /\ y[i] + h[i] <= 8);

constraint diffn_nonstrict(x, y, w, h);

solve satisfy;
