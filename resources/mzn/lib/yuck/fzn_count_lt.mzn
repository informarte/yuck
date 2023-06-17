predicate yuck_count_lt_bool(array [int] of var bool: x, var bool: y, var int: c);

predicate fzn_count_lt(array [int] of var bool: x, var bool: y, var int: c) =
    yuck_count_lt_bool(x, y, c);

predicate yuck_count_lt_int(array [int] of var int: x, var int: y, var int: c);

predicate fzn_count_lt(array [int] of var int: x, var int: y, var int: c) =
    yuck_count_lt_int(x, y, c);

predicate yuck_count_lt_set(array [int] of var set of int: x, var set of int: y, var int: c);

predicate fzn_count_lt(array [int] of var set of int: x, var set of int: y, var int: c) =
    yuck_count_lt_set(x, y, c);

% The MiniZinc library does not support set counting.
predicate count_lt(array [int] of var set of int: x, var set of int: y, var int: c) =
    fzn_count_lt(x, y, c);
