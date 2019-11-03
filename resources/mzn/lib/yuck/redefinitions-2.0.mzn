% This file contains redefinitions of standard builtins that can be overridden
% by solvers.

predicate bool_clause_reif(
    array [int] of var bool: as, array [int] of var bool: bs,
    var bool: satisfied);

predicate array_int_maximum(var int: m, array [int] of var int: x);

predicate array_float_maximum(var float: m, array [int] of var float: x);

predicate array_int_minimum(var int: m, array [int] of var int: x);

predicate array_float_minimum(var float: m, array [int] of var float: x);
