include "table_int_test_scenario.mzn";
include "table.mzn";

constraint bool2int(table(x, t), 1);

solve satisfy;
