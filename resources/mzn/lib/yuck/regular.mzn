% See table_int.mzn for an explanation of what is going on here.

predicate yuck_regular(
    array[int] of var int: x, int: Q, int: S,
    array[int] of int: d, int: q0, set of int: F);

predicate regular(
    array[int] of var int: x, int: Q, int: S,
    array[int, int] of int: d, int: q0, set of int: F) =
    assert(
        Q > 0,
        "regular: 'Q' must be greater than zero",
        assert(
            S > 0,
            "regular: 'S' must be greater than zero",
            assert(
                index_set_1of2(d) = 1..Q /\ index_set_2of2(d) == 1..S,
                "regular: the transition function 'd' must be [1..Q,1..S]",
                assert(
                    forall([d[i, j] in 0..Q | i in 1..Q, j in 1..S]),
                    "regular: transition function 'd' points to states outside 0..Q",
                    % Nb: we need the parentheses around the expression otherwise the
                    % parser thinks it's a generator call!
                    assert(
                        (q0 in 1..Q),
                        "regular: start state 'q0' not in 1..Q",
                        assert(
                            F subset 1..Q,
                            "regular: final states in 'F' contain states outside 1..Q",
                            yuck_regular(x, Q, S, [d[i, j] | i in index_set_1of2(d), j in index_set_2of2(d)], q0, F)))))));
