include "global_cardinality.mzn";

predicate global_cardinality_low_up(
    array[int] of var int: x,
    array[int] of int: cover,
    array[int] of int: lbound,
    array[int] of int: ubound) =
    assert(
        index_set(cover) = index_set(lbound) /\ index_set(lbound) = index_set(ubound),
        "global_cardinality_low_up: cover, lbound, and ubound must have identical index sets",
        let {array[index_set(cover)] of var int: counts}
         in forall (i in index_set(cover)) (counts[i] in lbound[i] .. ubound[i])
            /\
            global_cardinality(x, cover, counts));
