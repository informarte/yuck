include "yuck_diffn.mzn";

predicate fzn_diffn(
    array [int] of var int: x, array [int] of var int: y,
    array [int] of var int: w, array [int] of var int: h)
=
    yuck_diffn(x, y, w, h, true);
