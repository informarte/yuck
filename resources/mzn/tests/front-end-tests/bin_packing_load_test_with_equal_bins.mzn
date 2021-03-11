% Tests the ability of the compiler to deal with duplicate bin variables in
% bin_packing_load constraints.

include "bin_packing_load.mzn";
include "bin_packing_load_test_scenario.mzn";

constraint forall(i in 1..B)(load[i] in 0..sum(weight));
constraint bin[1] = bin[N];
constraint bin_packing_load(load, bin, weight);

solve satisfy;
