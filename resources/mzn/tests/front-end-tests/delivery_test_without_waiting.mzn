include "delivery.mzn";

include "cvrp.mzn";
include "A-n37-k5.dzn";

predicate track_times() =
    delivery(StartNodes, EndNodes, succ, arrivalTimes, [], GiantTourTravelTimes, false, totalTravelTime);

predicate track_loads() =
    delivery(StartNodes, EndNodes, succ, loads, GiantTourDemands, [||], false, 0);

solve minimize totalTravelTime;

output [
    "totalTravelTime = ", show(totalTravelTime), ",\n",
    "arrivalTimes = ", show(arrivalTimes), ",\n",
    "loads = ", show(loads), ",\n",
    "succ = ", show(succ)];
