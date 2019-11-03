include "diffn_strict_test_scenario.mzn";
include "diffn.mzn";

constraint bool2int(diffn(x, y, w, h), 1);

solve satisfy;
