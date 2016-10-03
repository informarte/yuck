include "cumulative.mzn";

predicate safe_bin_packing(
    int: c,
    array[int] of var int: bin,
    array[int] of int: w,
    var bool: continuation) =
    assert(
        index_set(bin) == index_set(w),
        "bin_packing: the bin and weight arrays must have identical index sets",
        assert(
           lb_array(w) >= 0,
           "bin_packing: the weights must be non-negative",
            assert(
                c >= 0,
                "bin_packing: capacity must be non-negative",
                continuation)));

predicate bin_packing(
    int: c,
    array[int] of var int: bin,
    array[int] of int: w) =
    safe_bin_packing(c, bin, w, cumulative(bin, [1 | i in index_set(bin)], w, c));
