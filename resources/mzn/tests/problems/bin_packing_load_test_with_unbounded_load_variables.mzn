% Tests the ability of the compiler to turn load variables in
% bin_packing_load constraints into channel variables.
% To this end, we do not define domains for the load variables.
% (Search variables need domains.)

include "bin_packing_load.mzn";

int: N = 6;
int: B = 3;

array [1..N] of var 1..B: bin;
array [1..N] of int: w = [i | i in 1..N];
array [1..B] of var int: load;

constraint bin_packing_load(load, bin, w);

solve satisfy;

output [
    " bin = ", show(bin), "\n",
    "load = ", show(load)]
