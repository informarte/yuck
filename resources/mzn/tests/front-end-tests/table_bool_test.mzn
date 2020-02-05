include "table_bool_test_scenario.mzn";
include "table.mzn";

constraint table(x, t);

solve satisfy;
