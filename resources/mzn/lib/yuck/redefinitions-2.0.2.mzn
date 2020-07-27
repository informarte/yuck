% This file contains redefinitions of standard builtins for version 2.0.2
% that can be overridden by solvers.


predicate symmetry_breaking_constraint(var bool: b) = true;

predicate redundant_constraint(var bool: b) = true;


predicate yuck_array_bool_element(int: offset, var int: i, array [int] of var bool: x, var bool: y);
predicate yuck_array_int_element(int: offset, var int: i, array [int] of var int: x, var int: y);
predicate yuck_array_float_element(int: offset, var int: i, array [int] of var float: x, var float: y);
predicate yuck_array_set_element(int: offset, var int: i, array [int] of var set of int: x, var set of int: y);

predicate array_var_bool_element_nonshifted(var int: idx, array [int] of var bool: x, var bool: c) =
    if is_fixed(idx) \/ min(index_set(x)) == 1
    then array_var_bool_element(idx - (min(index_set(x)) - 1), x, c) :: defines_var(c)
    else yuck_array_bool_element(min(index_set(x)), idx, x, c) :: defines_var(c)
    endif;

predicate array_var_int_element_nonshifted(var int: idx, array [int] of var int: x, var int: c) =
    if is_fixed(idx) \/ min(index_set(x)) == 1
    then array_var_int_element(idx - (min(index_set(x)) - 1), x, c) :: defines_var(c)
    else yuck_array_int_element(min(index_set(x)), idx, x, c) :: defines_var(c)
    endif;

predicate array_var_float_element_nonshifted(var int: idx, array [int] of var float: x, var float: c) =
    if is_fixed(idx) \/ min(index_set(x)) == 1
    then array_var_float_element(idx - (min(index_set(x)) - 1), x, c) :: defines_var(c)
    else yuck_array_float_element(min(index_set(x)), idx, x, c) :: defines_var(c)
    endif;

predicate array_var_set_element_nonshifted(var int: idx, array [int] of var set of int: x, var set of int: c) =
    if is_fixed(idx) \/ min(index_set(x)) == 1
    then array_var_set_element(idx - (min(index_set(x)) - 1), x, c) :: defines_var(c)
    else yuck_array_set_element(min(index_set(x)), idx, x, c) :: defines_var(c)
    endif;
