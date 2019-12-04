include "diffn_strict_test_scenario.mzn";
include "diffn.mzn";

constraint diffn(x, y, w, h);

solve satisfy;
