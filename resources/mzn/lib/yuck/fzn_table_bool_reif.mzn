predicate yuck_table_bool_reif(
    array [int] of var bool: x, array [int] of bool: t,
    var bool: satisfied);

predicate fzn_table_bool_reif(
    array [int] of var bool: x, array [int, int] of bool: t,
    var bool: satisfied) =
    yuck_table_bool_reif(x, array1d(t), satisfied);
