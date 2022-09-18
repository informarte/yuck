include "fzn_count_leq.mzn";

predicate fzn_member_int(array [int] of var int: x, var int: y) =
    fzn_count_leq(x, y, 1);
