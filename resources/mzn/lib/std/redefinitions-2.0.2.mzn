% This file contains redefinitions of standard builtins for version 2.0.2
% that can be overridden by solvers.


% Redefined to facilitate the verification of solutions found by YUCK
% (as suggested by Andreas Schutt/NICTA)
predicate symmetry_breaking_constraint(var bool: b) = true;
predicate redundant_constraint(var bool: b) = true;



predicate array_var_bool_element_nonshifted(var int: idx, array[int] of var bool: x, var bool: c) =
  array_var_bool_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);

predicate array_var_int_element_nonshifted(var int: idx, array[int] of var int: x, var int: c) =
  array_var_int_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);

predicate array_var_float_element_nonshifted(var int: idx, array[int] of var float: x, var float: c) =
  array_var_float_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);

predicate array_var_set_element_nonshifted(var int: idx, array[int] of var set of int: x, var set of int: c) =
  array_var_set_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);


% Implementation of int_pow for G12/FD
% (taken from https://github.com/MiniZinc/libminizinc/issues/168)

predicate int_pow(var int: x, var int: y, var int: r) = (
    let {
        array[dom(y)] of var int: x2y = array1d(dom(y),
            [int_pow_aux(x, cy) | cy in dom(y)]);
        constraint (y < 0 -> (x = 1 \/ x = -1));
    } in r = x2y[y]
);

function var int: int_pow_aux(var int: x, int: y) =
    if y < 0 \/ y = 1 then
        % Note that this is only correct in the combination with the constraint (y < 0 -> (x = 1 \/ x = -1)) for a negative power
        x
    elseif y = 0 then
        1
    else
        x * int_pow_aux(x, y - 1)
    endif;
