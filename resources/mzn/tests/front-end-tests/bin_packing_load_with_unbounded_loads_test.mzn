% Tests the ability of the compiler to turn load variables in
% bin_packing_load constraints into channel variables.

include "bin_packing_load.mzn";
include "bin_packing_load_test_scenario.mzn";

constraint forall(i in 1..B)(load[i] in 0..sum(weight));
constraint bin_packing_load(load, bin, weight);

solve satisfy;
