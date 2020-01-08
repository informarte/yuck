include "global_cardinality_low_up_test_scenario.mzn";
include "global_cardinality_low_up.mzn";

constraint x[3] = 8;

constraint global_cardinality_low_up(x, cover, lb, ub);

solve satisfy;
