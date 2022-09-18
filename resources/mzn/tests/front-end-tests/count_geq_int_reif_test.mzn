include "count_int_reif_test_scenario.mzn";
include "count_geq.mzn";

predicate count_op(array [int] of var int: x, var int: y, var int: c) =
    count_geq(x, y, c);
