predicate yuck_disjoint2(
    array[int] of var int: x, array[int] of var int: y,
    array[int] of var int: w, array[int] of var int: h,
    bool: strict);

predicate disjunctive(
    array[int] of var int: s, array[int] of var int: d, bool: strict) =
    assert(
        index_set(s) == index_set(d),
        "disjunctive: index set mismatch",
        assert(
           lb_array(d) >= 0,
           "disjunctive: the durations must be non-negative",
           yuck_disjoint2(s, [0 | i in index_set(s)], d, [1 | i in index_set(s)], strict)));

predicate disjunctive(array[int] of var int: s, array[int] of var int: d) =
    disjunctive(s, d, false);
