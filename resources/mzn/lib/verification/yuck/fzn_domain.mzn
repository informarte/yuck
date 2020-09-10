predicate fzn_domain(array[int] of var int: x, set of int: d) =
    forall(i in index_set(x))(x[i] in d);
