include "disjunctive_test_scenario.mzn";
include "disjunctive_strict.mzn";

constraint disjunctive_strict(o, d);

solve satisfy;
