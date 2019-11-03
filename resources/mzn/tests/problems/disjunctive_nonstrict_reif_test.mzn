include "disjunctive_test_scenario.mzn";
include "disjunctive.mzn";

constraint bool2int(disjunctive(o, d), 1);

solve satisfy;
