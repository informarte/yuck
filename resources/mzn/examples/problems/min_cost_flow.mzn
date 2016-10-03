% RUNS ON mzn_mer_lp
% RUNS ON mzn_fdlp
% RUNS ON mzn_mer_fdlp
% RUNS ON zinc_mip_hl
%------------------------------------------------------------------------------%
% min_cost_flow.mzn
% Jakob Puchinger <jakobp@cs.mu.oz.au>
% Wed Jun 14
%------------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Minimum Cost Flow problem.
% One of the most classic OR problems known: Find the minimum cost
% flow in a network, while satisfying the demands in the nodes,
% and not violating the capacities of the arcs.
%
% Testdata available at:
% http://elib.zib.de/pub/Packages/mp-testdata/mincost/ 
%
%-----------------------------------------------------------------------------%

    % number of nodes
int: n;

    % number of arcs
int: m;

    % flow demand at every node
array [1..n] of float: demand;

    % arc descriptors
array [1..m, 1..2] of int: arcs;

    % arc variables
array [1..m] of var float: X;

    % costs
array[1..m] of float: costs;

array[1..m] of float: capacity_lb;
array[1..m] of float: capacity;

    % Objective
solve minimize sum([ costs[i] * X[i] | i in 1..m ]);

    % Capacity constraints
constraint
    forall(i in 1..m)(
        X[i] <= capacity[i]
    );
    
    % Capacity LB
constraint
    forall(i in 1..m)(
        X[i] >= capacity_lb[i] 
    );

    % Satisfy demands
constraint
    forall(j in 1..n)
    (
        sum( [ X[i] | i in 1..m where arcs[i,2] = j ] ) - 
        sum( [ X[i] | i in 1..m where arcs[i,1] = j ] ) = demand[j]
    );

    % XXX: this output item should be improved eventually
output ["min_cost_flow: X = ", show(X), "\n"];

% toy data

n = 5;
m = 10;

demand = [-10.0, 0.0, 0.0, 0.0, 10.0];
arcs = [| 1, 2,
        | 1, 3,
        | 1, 4,
        | 1, 5,
        | 2, 3,
        | 2, 4,
        | 2, 5,
        | 3, 4,
        | 3, 5,
        | 4, 5 |];

capacity = [ 6.0, 4.0, 4.0, 4.0,
                3.0, 3.0, 3.0,
                   3.0, 3.0,
                      4.0];

capacity_lb = [ 0.0, 0.0, 0.0, 0.0,
                0.0, 0.0, 0.0,
                   0.0, 0.0,
                      0.0];

costs = [ 10.0, 6.0, 10.0, 20.0,
              2.0,  4.0, 10.0,
                  2.0, 10.0,
                      2.0];
