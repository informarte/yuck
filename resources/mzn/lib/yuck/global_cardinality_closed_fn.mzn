include "global_cardinality.mzn";
include "yuck.mzn";

function array[int] of var int: global_cardinality_closed(
    array [int] of var int: x,
    array [int] of int: cover) :: promise_total
=
    let {
        array[index_set(cover)] of var 0..length(x): count :: is_defined_var;
        constraint forall(i in index_set(x))(x[i] in {a | a in cover});
        constraint global_cardinality(x, cover, count) :: defines_vars(count);
    }
    in count;
