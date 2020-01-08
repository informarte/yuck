include "all_equal.mzn";
include "bin_packing_load_fn.mzn";
include "bin_packing_load_test_scenario.mzn";

constraint all_equal(load);
constraint load = bin_packing_load(bin, weight);

solve satisfy;
