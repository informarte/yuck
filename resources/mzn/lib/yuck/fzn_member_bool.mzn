include "fzn_count_leq_bool.mzn";

predicate fzn_member_bool(array [int] of var bool: x, var bool: y) =
    fzn_count_leq_bool(x, y, 1);
