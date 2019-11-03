predicate yuck_bin_packing_load_reif(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: w,
    int: minLoadIndex,
    var bool: satisfied);

predicate fzn_bin_packing_load_reif(
    array [int] of var int: load,
    array [int] of var int: bin,
    array [int] of int: w,
    var bool: satisfied)
=
    let {
        var bool: innerSatisfied :: is_defined_var;
        constraint yuck_bin_packing_load_reif(
            load, bin, w, min(index_set(load)), innerSatisfied) :: defines_var(innerSatisfied);
    } in (
        satisfied <-> (
            forall(i in index_set(bin))(
                min(index_set(load)) <= bin[i] /\ bin[i] <= max(index_set(load))
            )
            /\
            innerSatisfied
        )
    );
