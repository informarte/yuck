include "global_cardinality_test_scenario.mzn";
include "global_cardinality.mzn";
include "yuck.mzn";

constraint bool2costs(global_cardinality(x, cover, c), 0);

solve satisfy;
