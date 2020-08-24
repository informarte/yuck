include "alldifferent.mzn";

predicate yuck_circuit(array [int] of var int: succ, int: offset);

predicate fzn_circuit(array [int] of var int: succ) =
    forall(i in index_set(succ))(succ[i] in index_set(succ) diff {i}) /\
    yuck_circuit(succ, min(index_set(succ)));
