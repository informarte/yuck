include "table_bool_test_scenario.mzn";
include "table.mzn";
include "yuck.mzn";

constraint table(x, t) :: implicit;

solve satisfy;
