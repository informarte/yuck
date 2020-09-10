include "alldifferent.mzn";

predicate yuck_circuit(array [int] of var int: succ, int: offset);

predicate fzn_circuit(array [int] of var int: succ) =
    yuck_circuit(succ, min(index_set(succ)));
