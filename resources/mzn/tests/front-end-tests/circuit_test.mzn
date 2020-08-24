include "circuit.mzn";
include "circuit_test_scenario.mzn";

N = 100;

constraint circuit(succ);
constraint sum(i in 1..10)(succ[i]) == 777;

solve satisfy;
