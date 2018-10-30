predicate yuck_global_cardinality(
    array[int] of var int: x,
    array[int] of int: cover,
    array[int] of var int: counts);

predicate global_cardinality(
    array[int] of var int: x,
    array[int] of int: cover,
    array[int] of var int: counts) =
    assert(
        index_set(cover) = index_set(counts),
        "global_cardinality: cover and counts must have identical index sets",
        yuck_global_cardinality(x, cover, counts));
