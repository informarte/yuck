include "global_cardinality_test_scenario.mzn";
include "global_cardinality_closed_fn.mzn";

constraint count = global_cardinality_closed(x, cover);

solve satisfy;
