include "yuck_disjunctive_reif.mzn";

predicate fzn_disjunctive_strict_reif(
    array [int] of var int: s, array [int] of var int: d,
    var bool: satisfied)
=
    yuck_disjunctive_reif(s, d, true, satisfied);
