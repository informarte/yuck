include "yuck_increasing_int_reif.mzn";

predicate fzn_strictly_increasing_int_reif(array [int] of var int: x, var bool: satisfied) =
    yuck_increasing_int_reif(x, true, satisfied);
