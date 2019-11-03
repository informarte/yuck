% Tests the ability of the compiler to deal with an objective variable which
% is constrained by an implicit constraint.
% (As the objective variable is already handled by the implicit constraint's
% neighbourhood, the compiler must not create another neighbourhood containing
% the objective variable.)

include "alldifferent.mzn";

% The higher N, the higher the reliability of this test.
% (The smaller N, the higher the probability that the objective variable
% gets assigned the optimium value before search and hence that search
% will be skipped.)
int: N = 512;

array [1..N] of var 1..N: x;

% This is the implicit constraint.
constraint alldifferent(x);

solve minimize x[1];

output ["x = ", show(x)];
