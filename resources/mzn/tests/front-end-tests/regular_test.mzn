include "regular_test_scenario.mzn";
include "regular.mzn";

constraint regular(roster, Q, S, d, q0, F);

constraint roster[1] = WORK;

solve maximize count(shift in roster)(shift = OFF);
