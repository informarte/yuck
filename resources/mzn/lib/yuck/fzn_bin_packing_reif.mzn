include "bin_packing_load_fn.mzn";

predicate fzn_bin_packing_reif(
    int: c,
    array [int] of var int: bin,
    array [int] of int: weight,
    var bool: satisfied)
=
    let {
        array [int] of var int: load = bin_packing_load(bin, weight);
    } in (
        satisfied <-> forall(i in index_set(load))(load[i] <= c)
    );
