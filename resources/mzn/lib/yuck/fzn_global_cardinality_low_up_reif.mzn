include "global_cardinality.mzn";

predicate fzn_global_cardinality_low_up_reif(
    array [int] of var int: x,
    array [int] of int: cover,
    array [int] of int: lb,
    array [int] of int: ub,
    var bool: satisfied)
=
    let {
        array [int] of var int: count = global_cardinality(x, cover);
    } in satisfied <-> (
        forall(i in index_set(cover))(count[i] in lb[i]..ub[i])
    );
