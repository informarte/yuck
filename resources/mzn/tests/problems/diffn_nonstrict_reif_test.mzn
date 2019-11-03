include "diffn_nonstrict_test_scenario.mzn";
include "diffn_nonstrict.mzn";

constraint bool2int(diffn_nonstrict(x, y, w, h), 1);

solve satisfy;
