include "regular.mzn";
array[1..40] of var 1..2: x;
constraint forall(d in 1..4)(x[d] = x[d + 36]);
constraint regular(x, 21, 2, array2d(1..21,1..2,[3,6,3,6,6,4,6,5,6,0,11,0,12,0,13,0,0,0,15,0,16,2,17,2,0,2,19,0,20,2,21,2,0,2,0,0,0,2,0,2,0,2]), 1, 1..21);

solve satisfy;

