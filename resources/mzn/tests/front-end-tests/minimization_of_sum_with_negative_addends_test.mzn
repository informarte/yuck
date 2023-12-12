array [1..9,1..9] of int: w;
array [1..9] of var 1..9: n;
array [1..9] of var 0..30: c;
var 0..270: objective;
constraint objective = sum(c);
constraint forall(i in 1..9)(c[i] = w[i, n[i]]);
solve minimize objective;

w = [|0,7,-1,6,-1,-2,-1,-4,-1|8,0,12,-1,14,-1,-1,-1,-1|-1,5,0,-1,-1,15,-1,-1,-1|7,-1,-1,0,9,-1,19,-1,-1|-1,10,-1,14,0,4,-1,18,-1|-1,-1,3,-1,15,0,-1,-1,13|-1,-1,-1,3,-1,-1,0,17,-1|-1,-1,-1,-1,10,-1,17,0,13|-11,-18,-10,-13,-21,11,-8,20,0|];

output ["objective = ", show(objective)];
