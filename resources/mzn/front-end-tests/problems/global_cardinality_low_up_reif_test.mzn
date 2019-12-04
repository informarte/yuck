include "global_cardinality_low_up_test_scenario.mzn";
include "global_cardinality_low_up.mzn";
include "yuck.mzn";

constraint x[3] = 8;

constraint bool2costs(global_cardinality_low_up(x, cover, lb, ub), 0);

solve satisfy;
