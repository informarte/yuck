include "disjunctive.mzn";

predicate disjunctive_strict(array[int] of var int: s, array[int] of var int: d) =
    disjunctive(s, d, true);
