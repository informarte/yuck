include "domain.mzn";

var 0..2: x;

constraint domain([x], 1..3);

solve minimize x;
