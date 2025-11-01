var 0..1000: x;
var 0..1000: y;

constraint x < y;

solve :: warm_start([x, y], [999, 1000]) minimize y;
