include "regular_test_scenario.mzn";
include "regular.mzn";

constraint regular(roster, Q, S, d, q0, F);

solve satisfy;
