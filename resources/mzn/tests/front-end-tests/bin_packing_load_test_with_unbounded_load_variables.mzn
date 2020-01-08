% Tests the ability of the compiler to turn load variables in
% bin_packing_load constraints into channel variables.
% To this end, we do not define domains for the load variables.
% (Search variables need domains.)

include "bin_packing_load.mzn";
include "bin_packing_load_test_scenario.mzn";

constraint bin_packing_load(load, bin, weight);

solve satisfy;
