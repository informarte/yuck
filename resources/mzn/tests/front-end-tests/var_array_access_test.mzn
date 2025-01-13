array [1..10] of var -100..100: i;
array [1..30] of var 0..100: x;

constraint forall(j in index_set(i), a in dom(i[j]) where a mod 2 = 0)(i[j] != a);
constraint sum(j in index_set(i))(x[i[j]]) = 666;

solve satisfy;

output [
    "i = ", show(i), "\n",
    "x = ", show(x)];
