include "alldifferent.mzn";

% In this test case, the domain of x does not fit into 64 bits, so all bit-set domains should
% be converted to alternative representations.

var set of 0..64: x;
var set of 10..12 union 27..29: y;

array [1..4] of var set of int: l = [1..62, x, y, 1..3 union 7..9];
constraint all_different(l);

solve satisfy;
