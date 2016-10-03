include "bin_packing_capa.mzn";

int: nItems = 6;
int: nBins = 3;

array[1..nItems] of var 1..nBins: bin;
array[1..nItems] of int: w = [i | i in 1..nItems];
array[1..nBins] of int: c = [5,7,10];

constraint bin_packing_capa(c, bin, w);

solve satisfy;

output [show(bin[i]) | i in index_set(bin)];
