% Tests the ability of the compiler to handle equal load variables in
% bin_packing_load constraints.

include "all_equal.mzn";
include "bin_packing_load.mzn";

int: nItems = 6;
int: nBins = 3;

array[1..nItems] of var 1..nBins: bin;
array[1..nItems] of int: w = [i | i in 1..nItems];
array[1..nBins] of var 0..7: load;

constraint all_equal(load);
constraint bin_packing_load(load, bin, w);

solve satisfy;

output [
    " bin = ", show(bin), "\n",
    "load = ", show(load)]
