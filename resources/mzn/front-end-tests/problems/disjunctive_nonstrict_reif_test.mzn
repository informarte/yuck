include "disjunctive_test_scenario.mzn";
include "disjunctive.mzn";
include "yuck.mzn";

constraint bool2costs(disjunctive(o, d), 0);

solve satisfy;
