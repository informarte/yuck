
annotation defines_vars(array[$I] of var opt $T: x);
annotation defines_var(array[$I] of var int: x) = defines_vars(x);
annotation defines_var(array[$I] of var bool: x) = defines_vars(x);
annotation defines_var(array[$I] of var set of int: x) = defines_vars(x);
