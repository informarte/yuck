include "global_cardinality.mzn";
include "yuck.mzn";

function array[int] of var int: global_cardinality(
    array [int] of var int: x,
    array [int] of int: cover) :: promise_total
=
    let {
        array[index_set(cover)] of var 0..length(x): count :: is_defined_var;
        constraint global_cardinality(x, cover, count) :: defines_var(count);
    }
    in count;
