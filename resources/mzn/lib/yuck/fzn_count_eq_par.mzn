include "fzn_count_eq.mzn";

predicate fzn_count_eq_par(array [int] of var bool: x, bool: y, int: c) =
    fzn_count_eq(x, y, c);

predicate fzn_count_eq_par(array [int] of var int: x, int: y, int: c) =
    fzn_count_eq(x, y, c);

predicate fzn_count_eq_par(array [int] of var set of int: x, set of int: y, int: c) =
    fzn_count_eq(x, y, c);
