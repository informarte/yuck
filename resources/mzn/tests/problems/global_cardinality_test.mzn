include "global_cardinality_test_scenario.mzn";
include "global_cardinality.mzn";

constraint global_cardinality(x, cover, count);

solve satisfy;
