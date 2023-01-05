include "bin_packing_capa.mzn";
include "bin_packing_test_scenario.mzn";
include "yuck.mzn";

array [1..B] of int: capacity = [5, 7, 10];

constraint bool2costs(bin_packing_capa(capacity, bin, weight), 0);

solve satisfy;
