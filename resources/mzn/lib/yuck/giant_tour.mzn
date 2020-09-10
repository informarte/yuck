include "circuit.mzn";
include "domain.mzn";

function bool: giant_tour_requirements_are_met(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ) :: promise_total
=
    let {
        set of int: Nodes = index_set(succ)
    }
    in StartNodes != {}
    /\ card(StartNodes) = card(EndNodes)
    /\ max(StartNodes) < min(EndNodes)
    /\ StartNodes subset Nodes
    /\ EndNodes subset Nodes
    /\ dom_array(succ) subset Nodes;

% Hamiltonian circuit where start nodes succeed end nodes
predicate giant_tour(
    set of int: StartNodes,
    set of int: EndNodes,
    array[int] of var int: succ)
=
    let {
        set of int: Nodes = index_set(succ),
        int: K = card(StartNodes)
    }
    in assert(
        giant_tour_requirements_are_met(StartNodes, EndNodes, succ),
        "giant tour requirements not met",
        circuit(succ)
        /\ forall(i in 1..(K - 1))(succ[min(EndNodes) + i - 1] = min(StartNodes) + i)
        /\ succ[max(EndNodes)] = min(StartNodes)
        /\ domain([succ[i] | i in Nodes diff EndNodes], Nodes diff StartNodes));
