include "bin_packing.mzn";

int: nItems = 5;
int: nBins = 2;

array[1..nItems] of var 1..nBins: bin;
array[1..nItems] of int: w = [1, 2, 3, 4, 5];
int: c = 10;

constraint bin_packing(c, bin, w);

solve satisfy;

output ["bin = ", show(bin)];
