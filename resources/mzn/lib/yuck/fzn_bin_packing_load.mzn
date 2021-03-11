predicate yuck_bin_packing_load(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: weight,
    int: minLoadIndex);

predicate fzn_bin_packing_load(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: weight)
=
    forall(i in index_set(bin))(
        min(index_set(load)) <= bin[i] /\ bin[i] <= max(index_set(load))
    )
    /\
    yuck_bin_packing_load(load, bin, weight, min(index_set(load))) :: defines_vars(load);
