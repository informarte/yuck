include "diffn_nonstrict_test_scenario.mzn";
include "diffn_nonstrict.mzn";
include "yuck.mzn";

constraint bool2costs(diffn_nonstrict(x, y, w, h), 0);

solve satisfy;
