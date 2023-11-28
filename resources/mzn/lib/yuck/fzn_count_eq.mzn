% Boolean counting

predicate yuck_count_eq_bool(array [int] of var bool: x, var bool: y, var int: c);

predicate fzn_count_eq(array [int] of var bool: x, var bool: y, var int: c) =
    yuck_count_eq_bool(x, y, c);

function var int: count_eq(array[$X] of var bool: x, var bool: y) :: promise_total =
    let { var 0..length(x): c :: is_defined_var; constraint fzn_count_eq(array1d(x), y, c) :: defines_var(c); } in c;

function var int: count(array[$X] of var bool: x, var bool: y) :: promise_total =
    count_eq(x, y);


% Integer counting

predicate yuck_count_eq_int(array [int] of var int: x, var int: y, var int: c);

predicate fzn_count_eq(array [int] of var int: x, var int: y, var int: c) =
    yuck_count_eq_int(x, y, c);

function var int: count_eq(array[$X] of var int: x, var int: y) :: promise_total =
    let { var 0..length(x): c :: is_defined_var; constraint fzn_count_eq(array1d(x), y, c) :: defines_var(c); } in c;

function var int: count(array[$X] of var int: x, var int: y) :: promise_total =
    count_eq(x, y);


% Set counting
% Notice that the MiniZinc library does not support set counting.

predicate yuck_count_eq_set(array [int] of var set of int: x, var set of int: y, var int: c);

predicate fzn_count_eq(array [int] of var set of int: x, var set of int: y, var int: c) =
    yuck_count_eq_set(x, y, c);

predicate count_eq(array [int] of var set of int: x, var set of int: y, var int: c) =
    fzn_count_eq(x, y, c);

function var int: count_eq(array[$X] of var set of int: x, var set of int: y) :: promise_total =
    let { var 0..length(x): c :: is_defined_var; constraint fzn_count_eq(array1d(x), y, c) :: defines_var(c); } in c;

function var int: count(array[$X] of var set of int: x, var set of int: y) :: promise_total =
    count_eq(x, y);
