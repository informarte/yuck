include "yuck_diffn_reif.mzn";

predicate fzn_diffn_nonstrict_reif(
    array [int] of var int: x, array [int] of var int: y,
    array [int] of var int: w, array [int] of var int: h,
    var bool: satisfied)
=
    yuck_diffn_reif(x, y, w, h, false, satisfied);
