include "bin_packing.mzn";
include "bin_packing_test_scenario.mzn";

int: capacity = 10;

constraint bin_packing(capacity, bin, weight);

solve :: warm_start(bin, [3, 2, 2, 1, 2, 1]) satisfy;
