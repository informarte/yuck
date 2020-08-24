include "circuit.mzn";
include "circuit_test_scenario.mzn";

N = 5;

constraint circuit(succ) \/ circuit(reverse(succ));

solve satisfy;
