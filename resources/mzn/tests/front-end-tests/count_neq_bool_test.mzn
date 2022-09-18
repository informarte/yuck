include "count_bool_test_scenario.mzn";
include "count_neq.mzn";

predicate count_op(array [int] of var bool: x, var bool: y, var int: c) =
   count_neq(x, y, c);
