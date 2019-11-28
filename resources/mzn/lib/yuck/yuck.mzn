predicate bool2costs(var bool: a, var int: b);

function var int: bool2costs(var bool: a) :: promise_total =
    let {
        var int: b :: is_defined_var;
        constraint bool2costs(a, b) :: defines_var(b);
    } in b;

annotation defines_vars(array[$I] of var $T: x);
