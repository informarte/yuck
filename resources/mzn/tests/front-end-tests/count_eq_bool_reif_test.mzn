include "count_bool_reif_test_scenario.mzn";
include "count_eq.mzn";

predicate count_op(array [int] of var bool: x, var bool: y, var int: c) =
   count_eq(x, y, c);
