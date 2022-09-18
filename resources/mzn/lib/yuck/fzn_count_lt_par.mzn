include "fzn_count_lt.mzn";

predicate fzn_count_lt_par(array [int] of var bool: x, bool: y, int: c) =
    fzn_count_lt(x, y, c);

predicate fzn_count_lt_par(array [int] of var int: x, int: y, int: c) =
    fzn_count_lt(x, y, c);

predicate fzn_count_lt_par(array [int] of var set of int: x, set of int: y, int: c) =
    fzn_count_lt(x, y, c);
