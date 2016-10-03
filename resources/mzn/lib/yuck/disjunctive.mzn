include "cumulative.mzn";

predicate safe_disjunctive(
    array[int] of var int: s,
    array[int] of var int: d,
    var bool: continuation) =
    assert(
        index_set(s) == index_set(d),
        "disjunctive: the array arguments must have identical index sets",
        continuation);

predicate disjunctive(
    array[int] of var int: s,
    array[int] of var int: d) =
    safe_disjunctive(
        s, d,
        forall (i in index_set(d)) (d[i] >= 0) /\
        cumulative(s, d, [1 | i in index_set(s)], 1));
