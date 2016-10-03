predicate yuck_inverse(
    array[int] of var int: f, int: fOffset,
    array[int] of var int: g, int: gOffset);

predicate inverse(array[int] of var int: f, array[int] of var int: g) =
    forall(i in index_set(f))(f[i] in index_set(g)) /\
    forall(j in index_set(g))(g[j] in index_set(f)) /\
    yuck_inverse(f, min(index_set(f)), g, min(index_set(g)));
