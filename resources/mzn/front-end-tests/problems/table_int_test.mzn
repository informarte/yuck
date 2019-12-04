include "table_int_test_scenario.mzn";
include "table.mzn";

constraint table(x, t);

solve satisfy;
