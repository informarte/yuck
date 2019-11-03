include "cumulative.mzn";

predicate fzn_bin_packing(
    int: c,
    array [int] of var int: bin,
    array [int] of int: w)
=
    cumulative(bin, [1 | i in index_set(bin)], w, c);
