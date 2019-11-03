include "bin_packing.mzn";

int: N = 5;
int: B = 2;

array [1..N] of var 1..B: bin;
array [1..N] of int: w = [1, 2, 3, 4, 5];
int: c = 10;

constraint bin_packing(c, bin, w) \/ bin_packing(c, bin, reverse(w));

solve satisfy;

output ["bin = ", show(bin)];
