include "bin_packing.mzn";
include "bin_packing_test_scenario.mzn";
include "yuck.mzn";

int: capacity = 10;

constraint bool2costs(bin_packing(capacity, bin, weight), 0);

solve satisfy;
