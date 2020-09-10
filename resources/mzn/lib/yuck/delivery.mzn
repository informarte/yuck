include "fzn_delivery.mzn";
include "fzn_delivery_reif.mzn";
include "giant_tour.mzn";

function bool: delivery_requirements_are_met(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes) :: promise_total
=
    let {
        set of int: Nodes = index_set(succ)
    }
    in giant_tour_requirements_are_met(StartNodes, EndNodes, succ)
    /\ index_set(arrivalTimes) = Nodes
    /\ (index_set(ServiceTimes) = {} \/ index_set(ServiceTimes) = Nodes)
    /\ ((index_set_1of2(TravelTimes) = {} /\ index_set_2of2(TravelTimes) = {}) \/
        (index_set_1of2(TravelTimes) = Nodes /\ index_set_2of2(TravelTimes) = Nodes));

/**
  Helps with solving single-depot vehicle-routing problems with or without
  time windows.

  Requires the model to be based on the giant-tour representation where,
  in addition to the city nodes, there is a pair of start and end nodes
  for each vehicle.

  Constrains the arrival times to consider the given travel times:

    delivery(StartNodes, EndNodes, succ, arrivalTimes, ServiceTimes, TravelTimes, WithWaiting, totalTravelTime)

  is equivalent to

    let {
        set of int: Nodes = index_set(succ)
    }
    in forall(i in Nodes diff EndNodes)(
        let {
            var int: j = succ[i],
            var int: t = arrivalTimes[i] + ServiceTimes[i] + TravelTimes[i, j],
            var int: u = arrivalTimes[j]
        }
        in if WithWaiting then u >= t else u = t endif
    )
    /\
    totalTravelTime = sum(i in Nodes diff EndNodes)(TravelTimes[i, succ[i]]);
*/
predicate delivery(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ,
    array[int] of var int: arrivalTimes,
    array[int] of int: ServiceTimes,
    array[int, int] of int: TravelTimes,
    bool: WithWaiting,
    var int: totalTravelTime)
=
    assert(
        delivery_requirements_are_met(StartNodes, EndNodes, succ, arrivalTimes, ServiceTimes, TravelTimes),
        "delivery requirements not met",
        fzn_delivery(StartNodes, EndNodes, succ, arrivalTimes, ServiceTimes, TravelTimes, WithWaiting, totalTravelTime));

/**
  Reified version of delivery
 */
predicate delivery_reif(
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
    assert(
        delivery_requirements_are_met(StartNodes, EndNodes, succ, arrivalTimes, ServiceTimes, TravelTimes),
        "delivery_reif requirements not met",
        fzn_delivery_reif(
            StartNodes, EndNodes, succ, arrivalTimes, ServiceTimes, TravelTimes, WithWaiting, totalTravelTime,
            satisfied));
