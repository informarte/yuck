include "global_cardinality.mzn";

predicate global_cardinality_closed(
    array[int] of var int: x,
    array[int] of int: cover,
    array[int] of var int: counts) =
    forall (i in index_set(x)) (x[i] in array2set(cover))
    /\
    global_cardinality(x, cover, counts);
