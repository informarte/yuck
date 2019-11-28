include "global_cardinality_fn.mzn";

predicate fzn_global_cardinality_low_up(
    array [int] of var int: x,
    array [int] of int: cover,
    array [int] of int: lb,
    array [int] of int: ub)
=
    let {
        array [int] of var int: count = global_cardinality(x, cover);
    } in (
        forall(i in index_set(cover))(count[i] in lb[i]..ub[i])
    );
