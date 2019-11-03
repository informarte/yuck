include "disjunctive_test_scenario.mzn";
include "disjunctive.mzn";

constraint disjunctive(o, d);

solve satisfy;
