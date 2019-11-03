predicate yuck_bin_packing_load(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: w,
    int: minLoadIndex);

predicate fzn_bin_packing_load(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: w)
=
    forall(i in index_set(bin))(
        min(index_set(load)) <= bin[i] /\ bin[i] <= max(index_set(load))
    )
    /\
    yuck_bin_packing_load(load, bin, w, min(index_set(load)));
