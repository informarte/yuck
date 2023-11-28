predicate yuck_table_int_reif(
    array [int] of var int: x, array [int] of int: t,
    var bool: satisfied);

predicate fzn_table_int_reif(
    array [int] of var int: x, array [int, int] of int: t,
    var bool: satisfied) =
    yuck_table_int_reif(x, array1d(t), satisfied);
