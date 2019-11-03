include "yuck_disjunctive.mzn";

predicate fzn_disjunctive(
    array [int] of var int: s, array [int] of var int: d)
=
    yuck_disjunctive(s, d, false);
