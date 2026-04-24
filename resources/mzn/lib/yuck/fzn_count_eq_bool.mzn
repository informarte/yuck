predicate fzn_count_eq_bool(array [int] of var bool: x, var bool: y, var int: c);

function var int: count_eq(array[$X] of var bool: x, var bool: y) :: promise_total =
    let { var 0..length(x): c :: is_defined_var; constraint fzn_count_eq_bool(array1d(x), y, c) :: defines_var(c); } in c;

function var int: count(array[$X] of var bool: x, var bool: y) :: promise_total =
    count_eq(x, y);
