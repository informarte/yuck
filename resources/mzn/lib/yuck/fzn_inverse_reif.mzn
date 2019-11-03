predicate yuck_inverse_reif(
    array [int] of var int: f, int: fOffset,
    array [int] of var int: g, int: gOffset,
    var bool: satisfied);

predicate fzn_inverse_reif(
    array [int] of var int: f,
    array [int] of var int: g,
    var bool: satisfied)
=
    let {
        var bool: innerSatisfied :: is_defined_var;
        constraint yuck_inverse_reif(
            f, min(index_set(f)), g, min(index_set(g)), innerSatisfied) :: defines_var(innerSatisfied);
    } in (
        satisfied <-> (
            forall(i in index_set(f))(f[i] in index_set(g)) /\
            forall(j in index_set(g))(g[j] in index_set(f)) /\
            innerSatisfied
        )
    );
