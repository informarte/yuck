include "cumulative.mzn";

predicate bin_packing(
    int: c,
    array[int] of var int: bin,
    array[int] of int: w) =
    assert(
        index_set(bin) == index_set(w),
        "bin_packing: the bin and weight arrays must have identical index sets",
        assert(
           lb_array(w) >= 0,
           "bin_packing: the weights must be non-negative",
            assert(
                c >= 0,
                "bin_packing: capacity must be non-negative",
                cumulative(bin, [1 | i in index_set(bin)], w, c))));
