include "bin_packing_load.mzn";
include "yuck.mzn";

function array[int] of var int: bin_packing_load(
    array[int] of var int: bin,
    array[int] of int: weight) :: promise_total
=
    let {
        array[dom_bounds_array(bin)] of var 0..sum(weight): load :: is_defined_var;
        constraint bin_packing_load(load, bin, weight) :: defines_vars(load);
    }
    in load;
