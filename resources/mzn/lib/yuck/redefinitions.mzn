mzn_half_reify_clause = false;

annotation yuck_defines_bool_vars(array[$I] of var bool: x);
annotation yuck_defines_int_vars(array[$I] of var int: x);
annotation yuck_defines_set_vars(array[$I] of var set of int: x);

annotation defines_var(array[$I] of var bool: x) = yuck_defines_bool_vars(x);
annotation defines_var(array[$I] of var int: x) = yuck_defines_int_vars(x);
annotation defines_var(array[$I] of var set of int: x) = yuck_defines_set_vars(x);
