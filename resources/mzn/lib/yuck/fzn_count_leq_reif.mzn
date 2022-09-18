predicate yuck_count_leq_bool_reif(array [int] of var bool: x, var bool: y, var int: c, var bool: satisfied);

predicate fzn_count_leq_reif(array [int] of var bool: x, var bool: y, var int: c, var bool: satisfied) =
    yuck_count_leq_bool_reif(x, y, c, satisfied);

predicate yuck_count_leq_int_reif(array [int] of var int: x, var int: y, var int: c, var bool: satisfied);

predicate fzn_count_leq_reif(array [int] of var int: x, var int: y, var int: c, var bool: satisfied) =
    yuck_count_leq_int_reif(x, y, c, satisfied);

predicate yuck_count_leq_set_reif(array [int] of var set of int: x, var set of int: y, var int: c, var bool: satisfied);

predicate fzn_count_leq_reif(array [int] of var set of int: x, var set of int: y, var int: c, var bool: satisfied) =
    yuck_count_leq_set_reif(x, y, c, satisfied);
