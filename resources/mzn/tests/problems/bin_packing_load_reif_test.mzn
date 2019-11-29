include "bin_packing_load.mzn";
include "bin_packing_load_test_scenario.mzn";

constraint forall(i in 1..B)(load[i] in 0..sum(weight));
constraint bin_packing_load(load, bin, weight) \/ bin_packing_load(reverse(load), bin, weight);

solve satisfy;
