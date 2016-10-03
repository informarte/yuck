% table_int takes the table as a two-dimensional array.
% Since FlatZinc only supports one dimension, the table has to be flattened.
% mzn2fzn has a built-in flattener but it does not transform constraint signatures
% and hence relying on it would result in an illegal FlatZinc file.
% In the transformation given below, we replace table_int by yuck_table_int
% to obtain a valid FlatZinc file.
% (Interestingly, the Gecode binding does not define such a step.
% So it seems that Gecode's FlatZinc parser ignores illegal predicate definitions.)

predicate yuck_table_int(
    array[int] of var int: x,
    array[int] of int: t);

predicate safe_table_int(
    array[int] of var int: x,
    array[int, int] of int: t,
    var bool: continuation) =
    assert(
        index_set_2of2(t) == index_set(x),
        "The second dimension of the table must equal the number of variables in the first argument",
        continuation);

predicate table_int(
    array[int] of var int: x,
    array[int, int] of int: t) =
    safe_table_int(
        x, t,
        yuck_table_int(x, [t[i, j] | i in index_set_1of2(t), j in index_set_2of2(t)]));
