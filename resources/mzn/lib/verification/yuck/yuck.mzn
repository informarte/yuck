% This is an approximation of Yuck's bool2costs predicate to facilitate
% verification in integration testing.
predicate bool2costs(var bool: a, var int: b) =
    bool2int(not a, b);

function var int: bool2costs(var bool: a) :: promise_total =
    let {
        var int: b;
        constraint bool2costs(a, b);
    } in b;
