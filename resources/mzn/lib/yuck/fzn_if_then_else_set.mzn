predicate yuck_if_then_else_set(array[int] of var bool: c, array[int] of var set of int: x, var set of int: y);

predicate fzn_if_then_else_set(array[int] of var bool: c, array[int] of var set of int: x, var set of int: y) =
    let {
        int: l = min(index_set(c));
        int: u = max(index_set(c))
    } in assert(
        index_set(c) == index_set(x) /\ is_fixed(c[u]) /\ fix(c[u]),
        "fzn_if_then_else_set requirements not met",
        if is_fixed(c[l])
        then if fix(c[l])
             then y = x[l]
             else fzn_if_then_else_set([c[i] | i in (l + 1)..u], [x[i] | i in (l + 1)..u], y)
             endif
        elseif forall(i in index_set(x))(x[i] = x[l])
        then y = x[l]
        else yuck_if_then_else_set(c, x, y) :: defines_var(y)
        endif);
