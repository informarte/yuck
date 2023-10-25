include "bin_packing.mzn";
include "bin_packing_test_scenario.mzn";

int: capacity = 10;

constraint forall(b in lb_array(bin)..ub_array(bin))(capacity >= sum(i in index_set(bin))(weight[i] * (bin[i] == b)));

constraint redundant_constraint(bin_packing(capacity, bin, weight));

solve satisfy;
