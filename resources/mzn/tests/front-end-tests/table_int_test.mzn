include "table_int_test_scenario.mzn";
include "table.mzn";
include "yuck.mzn";

constraint table(x, t) :: implicit;

solve satisfy;
