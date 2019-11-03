include "cumulative.mzn";

predicate fzn_bin_packing_capa_reif(
    array [int] of int: c,
    array [int] of var int: bin,
    array [int] of int: w,
    var bool: satisfied)
=
    satisfied <-> (
        forall(i in index_set(bin))(
            min(index_set(c)) <= bin[i] /\ bin[i] <= max(index_set(c))
        )
        /\
        let {
            int: maxc = max(c);
            array [int] of int: fixed = [i | i in index_set(c) where c[i] < maxc];
        } in (
            cumulative(
                bin ++ fixed,
                [1 | i in bin ++ fixed],
                w ++ [maxc - c[i] | i in fixed],
                maxc)
        )
    );
