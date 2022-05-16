include "bin_packing.mzn";
include "bin_packing_test_scenario.mzn";

int: capacity = 10;

constraint bin_packing(capacity, bin, weight);

solve ::
    seq_search(
        [warm_start([bin[1], bin[2]], [3, 2]),
         warm_start_array([warm_start([bin[3], bin[4]], [2, 1]),
                           warm_start([bin[5], bin[6]], [2, 1])])])
      satisfy;
