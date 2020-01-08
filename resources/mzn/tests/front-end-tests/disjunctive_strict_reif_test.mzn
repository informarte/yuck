include "disjunctive_test_scenario.mzn";
include "disjunctive_strict.mzn";
include "yuck.mzn";

constraint bool2costs(disjunctive_strict(o, d), 0);

solve satisfy;
