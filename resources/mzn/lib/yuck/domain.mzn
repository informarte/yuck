include "fzn_domain.mzn";

predicate domain(array[int] of var int: x, set of int: d) =
    fzn_domain(x, d);
