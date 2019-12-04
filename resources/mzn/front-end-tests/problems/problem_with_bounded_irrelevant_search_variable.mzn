var 1..10: x;
var bool: r :: is_defined_var;

constraint int_eq_reif(x, 5, r) :: defines_var(r);

solve satisfy;

output ["x = ", show(x)];
