% Based on an example from the Global Constraint Catalog, 2nd Edition, Revision A

% The example has four tasks to the placed within a tight timeframe.

% We add zero-duration events to check that these get placed correctly, too.

include "disjunctive_strict.mzn";

int: N = 10;

array [1..N] of var int: o;
array [1..N] of var int: d;

% tasks from the example
constraint o[1] in 2..5 /\ d[1] in 2..4;
constraint o[2] in 2..4 /\ d[2] in 1..6;
constraint o[3] in 3..6 /\ d[3] in 4..4;
constraint o[4] in 2..7 /\ d[4] in 1..3;

% additional zero-duration events
% - five events that must not coincide, so one event for each border and one for
%   each joint of above, regular tasks
% - one event that can coincide with any of the other five events
constraint forall(i in 5..10)(o[i] in 2..10 /\ d[i] = 0);
constraint forall(i in 5..8)(o[i] < o[i + 1]);

constraint disjunctive_strict(o, d);

solve satisfy;

output [
    "o = ", show(o), "\n",
    "d = ", show(d)];
