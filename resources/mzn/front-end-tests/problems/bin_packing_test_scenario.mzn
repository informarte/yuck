int: N = 6;
int: B = 3;

array [1..N] of var 1..B: bin;
array [1..N] of int: weight = [i | i in 1..N];

output ["bin = ", show(bin)];
