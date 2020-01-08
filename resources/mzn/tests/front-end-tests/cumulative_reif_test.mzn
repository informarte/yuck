include "cumulative_test_scenario.mzn";
include "cumulative.mzn";

constraint cumulative(o, d, h, r) \/ cumulative(o, d, h, r + 1);

solve satisfy;
