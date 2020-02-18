include "bin_packing_load_fn.mzn";
include "bin_packing_load_test_scenario.mzn";
include "yuck.mzn";

constraint load = bin_packing_load(bin, weight);

constraint load[1] <= 10;
constraint load[3] >= 3;

solve :: goal_hierarchy([int_max_goal(load[1]), int_max_goal(load[2]), int_max_goal(load[3])]) satisfy;
