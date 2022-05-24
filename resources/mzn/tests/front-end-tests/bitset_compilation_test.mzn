include "alldifferent.mzn";

var set of 0..63: x;
var set of 10..12 union 27..29: y;

array [1..4] of var set of int: l = [1..62, x, y, 1..3 union 7..9];
constraint all_different(l);

solve satisfy;
