include "yuck_increasing_int.mzn";

predicate fzn_strictly_increasing_int(array [int] of var int: x) =
    yuck_increasing_int(x, true);
