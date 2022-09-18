include "count_set_test_scenario.mzn";
include "count_gt.mzn";

predicate count_op(array [int] of var set of int: x, var set of int: y, var int: c) =
    count_gt(x, y, c);
