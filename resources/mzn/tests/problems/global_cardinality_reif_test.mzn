include "global_cardinality_test_scenario.mzn";
include "global_cardinality.mzn";

constraint bool2int(global_cardinality(x, cover, c), 1);

solve satisfy;
