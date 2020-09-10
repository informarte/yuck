% TSP with time windows

include "vrp.mzn";

% one vehicle
MinK = 1;
MaxK = 1;

% arrival times
array[1..2, {DepotNode} union CityNodes] of int: TimeWindows;
constraint forall(i in StartNodes)(arrivalTimes[i] = TimeWindows[1, DepotNode]);
constraint forall(i in EndNodes)(arrivalTimes[i] in TimeWindows[1, DepotNode]..TimeWindows[2, DepotNode]);
constraint forall(i in CityNodes)(arrivalTimes[i] in TimeWindows[1, i]..TimeWindows[2, i]);
