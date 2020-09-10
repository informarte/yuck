include "delivery.mzn";
include "yuck.mzn";

include "tsptw.mzn";
include "n20w20.001.dzn";

predicate track_times() =
    bool2costs(
        delivery(StartNodes, EndNodes, succ, arrivalTimes, [], GiantTourTravelTimes, true, totalTravelTime),
        0);

solve satisfy;

output [
    "totalTravelTime = ", show(totalTravelTime), ",\n",
    "arrivalTimes = ", show(arrivalTimes), ",\n",
    "succ = ", show(succ)];
