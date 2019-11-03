include "regular_test_scenario.mzn";
include "regular.mzn";

constraint bool2int(regular(roster, Q, S, d, q0, F), 1);

solve satisfy;
