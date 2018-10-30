include "alldifferent.mzn";

predicate yuck_cumulative(
    array[int] of var int: s,
    array[int] of var int: d,
    array[int] of var int: r,
    var int: b);

predicate cumulative(
    array[int] of var int: s,
    array[int] of var int: d,
    array[int] of var int: r,
    var int: b) =
    assert(
        index_set(s) == index_set(d) /\ index_set(s) == index_set(r),
        "cumulative: the array arguments must have identical index sets",
        assert(
            lb_array(d) >= 0 /\ lb_array(r) >= 0,
            "cumulative: durations and resource usages must be non-negative",
            if forall(i in index_set(r))(is_fixed(r[i]) /\ fix(r[i]) == 1) /\ is_fixed(b) /\ fix(b) == 1 then
               if forall(i in index_set(d))(is_fixed(d[i]) /\ fix(d[i]) == 1) then
                   alldifferent(s)
               else
                   % disjunctive case
                   yuck_cumulative(s, d, r, b)
               endif
            else
               yuck_cumulative(s, d, r, b)
            endif));
