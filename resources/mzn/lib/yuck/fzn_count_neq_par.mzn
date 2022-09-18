include "fzn_count_neq.mzn";

predicate fzn_count_neq_par(array [int] of var bool: x, bool: y, int: c) =
    fzn_count_neq(x, y, c);

predicate fzn_count_neq_par(array [int] of var int: x, int: y, int: c) =
    fzn_count_neq(x, y, c);

predicate fzn_count_neq_par(array [int] of var set of int: x, set of int: y, int: c) =
    fzn_count_neq(x, y, c);
