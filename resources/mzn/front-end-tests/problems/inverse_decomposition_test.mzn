% Tests the ability of the compiler to decompose an inverse constraint
% such that implicit solving can be applied.

include "inverse.mzn";

int: N = 100;
int: N_HALF = N div 2;

array [1..N] of var 1..N: f;
array [1..N] of var 1..N: g;

constraint assert(N_HALF * 2 == N, "N is not even", true);

constraint forall(i in 1..N_HALF)(f[i] <= N_HALF /\ g[i] <= N_HALF);
constraint forall(i in (N_HALF + 1)..N)(f[i] > N_HALF /\ g[i] > N_HALF);

constraint inverse(f, g);

constraint sum(i in 1..10)(g[i]) == 222;
constraint sum(i in (N_HALF + 1)..(N_HALF + 11))(g[i]) == 666;

solve satisfy;

output [
    "f = ", show(f), "\n",
    "g = ", show(g)];
