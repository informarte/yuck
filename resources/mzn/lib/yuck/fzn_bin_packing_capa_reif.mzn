include "bin_packing_load_fn.mzn";

predicate fzn_bin_packing_capa_reif(
    array [int] of int: capacity,
    array [int] of var int: bin,
    array [int] of int: weight,
    var bool: satisfied)
=
    satisfied <-> (
        forall(i in index_set(bin))(
            bin[i] in index_set(capacity)
        )
        /\
        let {
            array [int] of var int: load = bin_packing_load(bin, weight);
        } in (
            forall(i in index_set(load))(load[i] <= capacity[i])
        )
    );
