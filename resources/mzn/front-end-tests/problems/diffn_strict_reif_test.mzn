include "diffn_strict_test_scenario.mzn";
include "diffn.mzn";
include "yuck.mzn";

constraint bool2costs(diffn(x, y, w, h), 0);

solve satisfy;
