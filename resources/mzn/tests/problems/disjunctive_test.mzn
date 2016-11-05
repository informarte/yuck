include "disjunctive.mzn";

% Tests that the translation of disjunctive to cumulative handles
% tasks with zero duration as expected.
% To this end, we create five regular tasks of duration one to five
% and one irregular task with zero duration and we constrain the
% planning horizon s.t. a solution is only possible when the
% irregular task is scheduled s.t. it starts while some other
% task is executed.

int: nRegularTasks = 5;
int: horizon = sum([i | i in 1..nRegularTasks]);

% (s[1], d[1]) is the irregular task with duration 0
array [1..nRegularTasks + 1] of var 1..horizon - nRegularTasks + 1: s;
array [1..nRegularTasks + 1] of int: d = [i | i in 0..nRegularTasks];

constraint disjunctive(s, d);

solve satisfy;

output ["s = ", show(s)];
