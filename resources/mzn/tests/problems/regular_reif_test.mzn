include "regular_test_scenario.mzn";
include "regular.mzn";
include "yuck.mzn";

constraint bool2costs(regular(roster, Q, S, d, q0, F), 0);

solve satisfy;
