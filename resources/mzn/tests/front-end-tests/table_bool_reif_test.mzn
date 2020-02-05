include "table_bool_test_scenario.mzn";
include "table.mzn";
include "yuck.mzn";

constraint bool2costs(table(x, t), 0);

solve satisfy;
