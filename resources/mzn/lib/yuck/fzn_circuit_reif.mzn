predicate yuck_circuit_reif(array [int] of var int: succ, int: offset, var bool: satisfied);

predicate fzn_circuit_reif(array [int] of var int: succ, var bool: satisfied) =
    yuck_circuit_reif(succ, min(index_set(succ)), satisfied);
