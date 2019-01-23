predicate yuck_disjoint2(
    array[int] of var int: x, array[int] of var int: y,
    array[int] of var int: w, array[int] of var int: h,
    bool: strict);

predicate diffn(
    array[int] of var int: x, array[int] of var int: y,
    array[int] of var int: w, array[int] of var int: h,
    bool: strict) =
    assert(
        index_set(x) = index_set(y)  /\
        index_set(x) = index_set(w) /\
        index_set(x) = index_set(h),
        "diffn: index set mismatch",
	assert(
           lb_array(w) >= 0 /\ lb_array(h) >= 0,
           "diffn: the widths and heights must be non-negative",
           yuck_disjoint2(x, y, w, h, strict)));

predicate diffn(
    array[int] of var int: x, array[int] of var int: y,
    array[int] of var int: w, array[int] of var int: h) =
    diffn(x, y, w, h, true);
