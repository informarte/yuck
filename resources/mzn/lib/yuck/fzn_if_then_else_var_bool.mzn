predicate yuck_if_then_else_var_bool(array[int] of var bool: c, array[int] of var bool: x, var bool: y);

predicate fzn_if_then_else_var_bool(array[int] of var bool: c, array[int] of var bool: x, var bool: y) =
    let {
        int: l = min(index_set(c));
        int: u = max(index_set(c))
    } in assert(
        index_set(c) == index_set(x) /\ is_fixed(c[u]) /\ fix(c[u]),
        "fzn_if_then_else_var_bool requirements not met",
        if is_fixed(c[l])
        then if fix(c[l])
             then y = x[l]
             else fzn_if_then_else_var_bool([c[i] | i in (l + 1)..u], [x[i] | i in (l + 1)..u], y)
             endif
        else yuck_if_then_else_var_bool(c, x, y) :: defines_var(y)
        endif);
