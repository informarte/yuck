% Tests the ability of the compiler to handle shared load variables in
% bin_packing_load constraints.

include "all_equal.mzn";
include "bin_packing_load.mzn";
include "bin_packing_load_test_scenario.mzn";

constraint forall(i in 1..B)(load[i] in 0..sum(weight) div 2);
constraint bin_packing_load(load, bin, weight) /\ bin_packing_load(reverse(load), bin, weight);

solve satisfy;
