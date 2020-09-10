include "yuck.mzn";

predicate yuck_delivery_reif(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ,
    int: offset,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes,
    bool: WithWaiting,
    var int: totalTravelTime,
    var bool: satisfied);

predicate fzn_delivery_reif(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes,
    bool: WithWaiting,
    var int: totalTravelTime,
    var bool: satisfied)
=
    let {
        int: offset = min(index_set(succ))
    }
    in yuck_delivery_reif(
        StartNodes, EndNodes, succ, offset, arrivalTimes, ServiceTimes, TravelTimes, WithWaiting, totalTravelTime,
        satisfied)
        :: defines_vars([arrivalTimes[i] | i in index_set(arrivalTimes) diff StartNodes])
        :: defines_var(totalTravelTime)
        :: defines_var(satisfied);
