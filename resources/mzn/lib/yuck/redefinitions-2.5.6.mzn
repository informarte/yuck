% This file contains redefinitions of standard builtins for version 2.5.6
% that can be overridden by solvers.

% predicate bool_clause_imp(array[int] of var bool: as, array[int] of var bool: bs, var bool: b) =
%  clause(as,bs++[b]);

% The above, original definition turns b into a search variable.
% By not providing bool_clause_imp, we preserve the pre-2.5.6 behaviour, i.e.
% the compiler will emit a bool_clause_reif constraint with b a functionally
% defined variable.
