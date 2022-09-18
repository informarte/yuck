include "count_int_test_scenario.mzn";
include "count_gt.mzn";

predicate count_op(array [int] of var int: x, var int: y, var int: c) =
    count_gt(x, y, c);
