% RUNS ON mzn_mer_fd
% RUNS ON mzn_mer_lp
% RUNS ON zinc_mip_hl
% RUNS ON zinc_fdic_mznlib
% RUNS ON minizinc_fd
% RUNS ON minizinc_cpx
% perfsq.mzn
% vim: ft=zinc ts=4 sw=4 et
% Ralph Becket
% Thu May 31 11:44:33 EST 2007
%
% Perfect squares: find a set of integers the sum of whose squares is
% itself a square.

include "count.mzn";
include "alldifferent_except_0.mzn";

int: z = 10;

array [0..z] of 0..z*z: sq = array1d(0..z, [x*x | x in 0..z]);

array [1..z] of var 0..z: s;            % Indices into sq.

var 1..z: k;                            % We are summing to sq[k];

% We want a set.
%
constraint alldifferent_except_0(s);

% We want the sum of the squares to be square.
%
constraint sum ( i in 1..z ) ( sq[s[i]] )  =  sq[k];

% We want the biggest such set.
%
var int: j;
constraint count(s, 0, j) :: defines_var(j);

solve :: int_search(s, first_fail, indomain, complete) minimize j;

output [
    "perfsq\n",
    "length = ", show(j), "\n",
    show(k), "^2  =  ",
    show(s[1]), "^2 + ",
    show(s[2]), "^2 + ",
    show(s[3]), "^2 + ",
    show(s[4]), "^2 + ",
    show(s[5]), "^2 + ",
    show(s[6]), "^2 + ",
    show(s[7]), "^2 + ",
    show(s[8]), "^2 + ",
    show(s[9]), "^2 + ",
    show(s[10]), "^2\n",
];
