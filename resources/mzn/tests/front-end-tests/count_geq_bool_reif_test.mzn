include "count_bool_reif_test_scenario.mzn";
include "count_geq.mzn";

predicate count_op(array [int] of var bool: x, var bool: y, var int: c) =
   count_geq(x, y, c);
