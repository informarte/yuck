include "diffn.mzn";

predicate diffn_nonstrict(
    array[int] of var int: x, array[int] of var int: y,
    array[int] of var int: w, array[int] of var int: h) =
    diffn(x, y, w, h, false);
