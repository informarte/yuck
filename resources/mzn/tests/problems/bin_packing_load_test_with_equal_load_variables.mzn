% Tests the ability of the compiler to handle equal load variables in
% bin_packing_load constraints.

include "all_equal.mzn";
include "bin_packing_load.mzn";

int: N = 6;
int: B = 3;

array [1..N] of var 1..B: bin;
array [1..N] of int: w = [i | i in 1..N];
array [1..B] of var 0..7: load;

constraint all_equal(load);
constraint bin_packing_load(load, bin, w);

solve satisfy;

output [
    " bin = ", show(bin), "\n",
    "load = ", show(load)]
