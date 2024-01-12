include "yuck_increasing_int.mzn";

predicate fzn_increasing_int(array [int] of var int: x) =
    yuck_increasing_int(x, false);
