predicate yuck_bin_packing_load_reif(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: weight,
    int: minLoadIndex,
    var bool: satisfied);

predicate fzn_bin_packing_load_reif(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: weight,
    var bool: satisfied)
=
    satisfied <-> (
        forall(i in index_set(bin))(
            bin[i] in index_set(load)
        )
        /\
        yuck_bin_packing_load(load, bin, weight, min(index_set(load)))
    );
