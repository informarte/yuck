% Scenario inspired by the Global Constraint Catalogue

array [1..4] of var -100..100: x;
array [1..3] of int: cover = [3, 5, 6];
array [1..3] of int: lb = [2, 0, 1];
array [1..3] of int: ub = [3, 2, 1];

output ["x = ", show(x)];
