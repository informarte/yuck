%-----------------------------------------------------------------------------%
% A tiny travelling salesman problem to test the sqrt and pow functions.
%
% Ralph Becket
% 08/2007
%-----------------------------------------------------------------------------%
% RUNS ON mzn_mer_lp

include "alldifferent.mzn";

int: n_cities = 4;
set of int: cities = 1..n_cities;

array [cities]         of float: x = [0.0, 1.0, 2.0, 2.0];
array [cities]         of float: y = [3.0, 1.0, 2.0, 0.0];
array [cities, cities] of float: d =
	array2d(cities, cities,
        [ sqrt((x[a] - x[b]) * (x[a] - x[b]) + (y[a] - y[b]) * (y[a] - y[b]))
        | a, b in cities
        ]
    );

array [cities] of var cities: visit;

var float: dist =
    sum ( i in cities where i < n_cities ) ( d[visit[i], visit[i + 1]] );

constraint alldifferent(visit);

solve satisfy;

output [
    "tiny TSP:\n",
    "total distance = ", show(dist), "\n",
    "tour = ",
    show(visit[1]), ", ",
    show(visit[2]), ", ",
    show(visit[3]), ", ",
    show(visit[4]), "\n"
];
