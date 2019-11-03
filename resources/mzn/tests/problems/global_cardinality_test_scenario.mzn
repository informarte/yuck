% Scenario inspired by the Global Constraint Catalogue

array [1..9] of var -100..100: x = [6, 7, _, 8, _, 9, _, 8, 6];
array [1..4] of var -100..100: c = [5, 1, 2, _];
array [1..4] of int: cover = [6, 7, 8, 9];

output [
    "x = ", show(x), "\n",
    "c = ", show(c)];
