% Scenario inspired by the Global Constraint Catalogue

array [1..9] of var -100..100: x = [6, 7, _, 8, _, 9, _, 8, 6];
array [1..4] of int: cover = [6, 7, 8, 9];
array [1..4] of var -100..100: count = [5, 1, 2, _];

output [
    "x = ", show(x), "\n",
    "cover = ", show(cover), "\n",
    "count = ", show(count)];
