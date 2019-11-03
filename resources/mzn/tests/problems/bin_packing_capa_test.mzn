include "bin_packing_capa.mzn";

int: N = 6;
int: B = 3;

array [1..N] of var 1..B: bin;
array [1..N] of int: w = [i | i in 1..N];
array [1..B] of int: c = [5, 7, 10];

constraint bin_packing_capa(c, bin, w);

solve satisfy;

output ["bin = ", show(bin)];
