include "disjunctive_test_scenario.mzn";
include "disjunctive_strict.mzn";

constraint bool2int(disjunctive_strict(o, d), 1);

solve satisfy;
