predicate yuck_regular_reif(
    array [int] of var int: x, int: Q, int: S,
    array [int] of int: d, int: q0, set of int: F,
    var bool: satisfied);

predicate fzn_regular_reif(
    array [int] of var int: x, int: Q, int: S,
    array [int, int] of int: d, int: q0, set of int: F,
    var bool: satisfied) =
    yuck_regular_reif(x, Q, S, array1d(d), q0, F, satisfied);
