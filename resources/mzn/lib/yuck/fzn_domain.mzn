predicate yuck_int_domain(array[int] of var int: x, set of int: d);

predicate fzn_domain(array[int] of var int: x, set of int: d) =
    yuck_int_domain(x, d);
