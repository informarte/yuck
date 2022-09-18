include "fzn_count_leq.mzn";

predicate fzn_member_bool(array [int] of var bool: x, var bool: y) =
    fzn_count_leq(x, y, 1);
