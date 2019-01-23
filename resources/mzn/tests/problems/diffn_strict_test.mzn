% Based on an example from the Global Constraint Catalog, 2nd Edition, Revision A

% The example consists of six rectangles and has only one solution, see p. 1040.

% We add additional vertical lines, horizontal lines, and points to check that
% these get placed correctly, too.

include "diffn.mzn";

int: N = 19;

array [1..N] of var 1..8: x;
array [1..N] of var 1..8: y;
array [1..N] of var 0..7: w;
array [1..N] of var 0..7: h;

% rectangles from the example
constraint w[1] = 2 /\ h[1] = 5;
constraint w[2] = 5 /\ h[2] = 1;
constraint w[3] = 2 /\ h[3] = 4;
constraint w[4] = 4 /\ h[4] = 2;
constraint w[5] = 3 /\ h[5] = 3;
constraint x[6] = 1 /\ y[6] = 1 /\ w[6] = 3 /\ h[6] = 3;

% additional vertical lines
constraint w[7] = 0 /\ h[7] = 7; % has to go to the left border
constraint w[8] = 0 /\ h[8] = 6; % has to got between rectangles 3 and 5
constraint w[9] = 0 /\ h[9] = 5; % has to got between rectangles 1 and 3
constraint w[10] = 0 /\ h[10] = 7; % has to go to the right border
constraint w[11] = 0 /\ h[11] = 7; % extra line that can go to the left or right border
constraint x[7] < x[8];
constraint x[8] < x[9];
constraint x[9] < x[10];

% additional horizontal lines
constraint w[12] = 7 /\ h[12] = 0; % has to go to the bottom
constraint w[13] = 4 /\ h[13] = 0; % has to go between rectangles 1/3 and 4
constraint w[14] = 3 /\ h[14] = 0; % has to go between rectangles 5 and 6
constraint w[15] = 5 /\ h[15] = 0; % has to go between rectangles 2 and 3/5
constraint w[16] = 7 /\ h[16] = 0; % has to go to the top
constraint w[17] = 7 /\ h[17] = 0; % extra line that can go to the bottom or top
constraint y[12] < y[13];
constraint y[13] < y[14];
constraint y[14] < y[15];
constraint y[15] < y[16];

% additional points
constraint w[18] = 0 /\ h[18] = 0;
constraint w[19] = 0 /\ h[19] = 0;
constraint x[18] < x[19];
constraint y[18] < y[19];

% bounding box
constraint forall(i in 1..N)(x[i] + w[i] <= 8 /\ y[i] + h[i] <= 8);

constraint diffn(x, y, w, h);

solve satisfy;
