include "global_cardinality.mzn";

predicate fzn_global_cardinality_low_up_reif(
    array [int] of var int: x,
    array [int] of int: cover,
    array [int] of int: lb,
    array [int] of int: ub,
    var bool: satisfied)
=
    let {
        array [index_set(cover)] of var int: counts;
    } in satisfied <-> (
        forall(i in index_set(cover))(counts[i] in lb[i]..ub[i]) /\
        global_cardinality(x, cover, counts)
    );
