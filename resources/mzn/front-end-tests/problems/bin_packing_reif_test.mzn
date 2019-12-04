include "bin_packing.mzn";
include "bin_packing_test_scenario.mzn";

int: capacity = 10;

constraint bin_packing(capacity, bin, weight) \/ bin_packing(capacity, bin, reverse(weight));

solve satisfy;
