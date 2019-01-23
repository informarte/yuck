% Based on an example from the Global Constraint Catalog, 2nd Edition, Revision A

% The example has four tasks to the placed within a tight timeframe.

include "disjunctive.mzn";

array [1..4] of var int: o;
array [1..4] of var int: d;

constraint o[1] in 2..5 /\ d[1] in 2..4;
constraint o[2] in 2..4 /\ d[2] in 1..6;
constraint o[3] in 3..6 /\ d[3] in 4..4;
constraint o[4] in 2..7 /\ d[4] in 1..3;

constraint disjunctive(o, d);

solve satisfy;

output [
    "o = ", show(o), "\n",
    "d = ", show(d)];
