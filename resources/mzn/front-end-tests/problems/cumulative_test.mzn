include "cumulative_test_scenario.mzn";
include "cumulative.mzn";

constraint r = 5;
constraint cumulative(o, d, h, r);

solve satisfy;
