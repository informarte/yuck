include "diffn_nonstrict_test_scenario.mzn";
include "diffn_nonstrict.mzn";

constraint diffn_nonstrict(x, y, w, h);

solve satisfy;
