include "bin_packing_load_fn.mzn";

predicate fzn_bin_packing(
    int: capacity,
    array [int] of var int: bin,
    array [int] of int: weight)
=
    let {
        array [int] of var int: load = bin_packing_load(bin, weight);
    } in (
        forall(i in index_set(load))(load[i] <= capacity)
    );
