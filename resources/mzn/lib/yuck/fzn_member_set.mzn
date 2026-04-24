include "fzn_count_leq_set.mzn";

predicate fzn_member_set(array [int] of var set of int: x, var set of int: y) =
    fzn_count_leq_set(x, y, 1);
