% Capacitated VRP

include "vrp.mzn";

% maximum number of vehicles/ tours
float: MaxKToMinKRatio;
MaxK = min(N, floor(MinK * MaxKToMinKRatio));

% arrival times
constraint forall(i in StartNodes)(arrivalTimes[i] = 0);

% demands
array [CityNodes] of int: Demands;
array[Nodes] of int: GiantTourDemands = [if i in CityNodes then Demands[i] else 0 endif | i in Nodes];

% vehicle capacities and loads
int: Capacity;
set of int: Load = 0..Capacity;
array[Nodes] of var Load: loads;
constraint forall(i in StartNodes)(loads[i] = 0);
predicate track_loads();
constraint track_loads();
