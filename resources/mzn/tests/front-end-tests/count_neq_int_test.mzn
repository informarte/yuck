include "count_int_test_scenario.mzn";
include "count_neq.mzn";

predicate count_op(array [int] of var int: x, var int: y, var int: c) =
    count_neq(x, y, c);
