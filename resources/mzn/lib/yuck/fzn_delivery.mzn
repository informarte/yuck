include "yuck.mzn";

predicate yuck_delivery(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ,
    int: offset,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes,
    bool: WithWaiting,
    var int: totalTravelTime);

predicate fzn_delivery(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes,
    bool: WithWaiting,
    var int: totalTravelTime)
=
    let {
        int: offset = min(index_set(succ))
    }
    in yuck_delivery(
        StartNodes, EndNodes, succ, offset, arrivalTimes, ServiceTimes, TravelTimes, WithWaiting, totalTravelTime)
        :: defines_vars([arrivalTimes[i] | i in index_set(arrivalTimes) diff StartNodes])
        :: defines_var(totalTravelTime);
