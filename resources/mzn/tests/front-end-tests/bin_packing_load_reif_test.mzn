include "bin_packing_load.mzn";
include "bin_packing_load_test_scenario.mzn";
include "yuck.mzn";

constraint forall(i in 1..B)(load[i] in 0..sum(weight) div 2);
constraint bool2costs(bin_packing_load(load, bin, weight), 0);

solve satisfy;
