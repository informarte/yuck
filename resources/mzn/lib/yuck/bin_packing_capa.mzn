include "cumulative.mzn";

predicate bin_packing_capa(
    array[int] of int: c,
    array[int] of var int: bin,
    array[int] of int: w) =
    assert(
        index_set(bin) = index_set(w),
        "bin_packing_capa: the bin and weight arrays must have identical index sets",
        assert(
            lb_array(w) >= 0,
            "bin_packing_capa: the weights must be non-negative",
            assert(
                lb_array(c) >= 0,
                "bin_packing_capa: the capacities must be non-negative",
                forall (i in index_set(bin)) (
                    min(index_set(c)) <= bin[i] /\ bin[i] <= max(index_set(c))
                )
                /\
                let {int: cMax = max(c),
                     array[int] of int: fixed = [i | i in index_set(c) where c[i] < cMax]} in
                cumulative(
                    bin ++ fixed,
                    [1 | i in bin ++ fixed],
                    w ++ [cMax - c[i] | i in fixed],
                    cMax))));

