predicate fzn_delivery_reif(
    set of int: StartNodes,
    set of int: CityNodes,
    array[int] of var int: succ,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes,
    bool: WithWaiting,
    var int: totalTravelTime,
    var bool: satisfied)
=
    satisfied
    <->
    let {
        set of int: Nodes = index_set(succ),
        bool: WithServiceTimes = index_set(ServiceTimes) != {},
        bool: WithTravelTimes = index_set_1of2(TravelTimes) != {}
    }
    in forall(i in Nodes diff EndNodes)(
        let {
            var int: j = succ[i],
            var int: st = if WithServiceTimes then ServiceTimes[i] else 0 endif,
            var int: tt = if WithTravelTimes then TravelTimes[i, j] else 0 endif,
            var int: t = arrivalTimes[i] + st + tt,
            var int: u = arrivalTimes[j]
        }
        in if WithWaiting then u >= t else u = t endif
    )
    /\
    (if WithTravelTimes
     then totalTravelTime = sum(i in Nodes diff EndNodes)(TravelTimes[i, succ[i]])
     else totalTravelTime = 0
     endif);
