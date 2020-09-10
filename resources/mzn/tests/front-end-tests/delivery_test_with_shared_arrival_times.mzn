include "delivery.mzn";

include "tsptw.mzn";
include "n20w20.001.dzn";

predicate track_times() =
    delivery(StartNodes, EndNodes, succ, arrivalTimes, [], GiantTourTravelTimes, true, totalTravelTime) /\
    not delivery(StartNodes, EndNodes, succ, reverse(arrivalTimes), [], GiantTourTravelTimes, true, totalTravelTime);

solve satisfy;

output [
    "totalTravelTime = ", show(totalTravelTime), ",\n",
    "arrivalTimes = ", show(arrivalTimes), ",\n",
    "succ = ", show(succ)];
