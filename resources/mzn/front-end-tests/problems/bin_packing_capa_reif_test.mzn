include "bin_packing_capa.mzn";
include "bin_packing_test_scenario.mzn";

array [1..B] of int: capacity = [5, 7, 10];

constraint bin_packing_capa(capacity, bin, weight) \/ bin_packing_capa(reverse(capacity), bin, weight);

solve satisfy;
