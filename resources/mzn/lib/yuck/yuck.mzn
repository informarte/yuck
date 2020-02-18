predicate bool2costs(var bool: a, var int: b);

function var int: bool2costs(var bool: a) :: promise_total =
    let {
        var int: b :: is_defined_var;
        constraint bool2costs(a, b) :: defines_var(b);
    } in b;

annotation defines_vars(array[$I] of var $T: x);

annotation int_min_goal(var int: x);
annotation int_max_goal(var int: x);
annotation sat_goal(var bool: b);
annotation goal_hierarchy(array[$I] of ann);
