% VRP model based on giant-tour representation

include "giant_tour.mzn";

% problem dimensions and derived constants
int: N; % number of cities
int: MinK; % minimum number of tours/ vehicles
int: MaxK; % maximum number of tours/ vehicles
int: DepotNode = 0; % index of depot node in input data
set of int: CityNodes = 1..N; % indices of city nodes
set of int: StartNodes = (N + 1)..(N + MaxK); % indices of artificial start nodes (one per vehicle/ tour)
set of int: EndNodes = (N + MaxK + 1)..(N + 2 * MaxK); % indices of artificial end nodes (one per vehicle/ tour)
set of int: Nodes = 1..(N + 2 * MaxK); % indices of all nodes excluding the real depot node

% Hamiltonian circuit where start nodes succeed end nodes
array[Nodes] of var Nodes: succ;
constraint giant_tour(StartNodes, EndNodes, succ);

% travel times
array[{DepotNode} union CityNodes, {DepotNode} union CityNodes] of int: TravelTimes;
array[Nodes, Nodes] of int: GiantTourTravelTimes = array2d(Nodes, Nodes, [
    if i in CityNodes /\ j in CityNodes then
        TravelTimes[i, j]
    elseif i in CityNodes /\ not (j in CityNodes) then
        TravelTimes[i, DepotNode]
    elseif not (i in CityNodes) /\ j in CityNodes then
        TravelTimes[DepotNode, j]
    else
        TravelTimes[DepotNode, DepotNode]
    endif
    | i, j in Nodes]);

% arrival times
array[Nodes] of var int: arrivalTimes;
var 0..sum(i in Nodes)(max([GiantTourTravelTimes[i, j] | j in Nodes])): totalTravelTime;
predicate track_times();
constraint track_times();
