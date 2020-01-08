include "global_cardinality_test_scenario.mzn";
include "global_cardinality_fn.mzn";

constraint count = global_cardinality(x, cover);

solve satisfy;
