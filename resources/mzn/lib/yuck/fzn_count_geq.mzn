predicate yuck_count_geq_bool(array [int] of var bool: x, var bool: y, var int: c);

predicate fzn_count_geq(array [int] of var bool: x, var bool: y, var int: c) =
    yuck_count_geq_bool(x, y, c);

predicate yuck_count_geq_int(array [int] of var int: x, var int: y, var int: c);

predicate fzn_count_geq(array [int] of var int: x, var int: y, var int: c) =
    yuck_count_geq_int(x, y, c);

predicate yuck_count_geq_set(array [int] of var set of int: x, var set of int: y, var int: c);

predicate fzn_count_geq(array [int] of var set of int: x, var set of int: y, var int: c) =
    yuck_count_geq_set(x, y, c);

% The MiniZinc library does not support set counting.
predicate count_geq(array [int] of var set of int: x, var set of int: y, var int: c) =
    fzn_count_geq(x, y, c);
