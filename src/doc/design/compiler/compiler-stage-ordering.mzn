include "all_different.mzn";

int: N = 10;

set of int: STAGES = 1..N;

var STAGES: DomainInitializer;
var STAGES: VariableFactory;
var STAGES: VariableClassifier;
var STAGES: ConstraintFactory;
var STAGES: CycleBreaker;
var STAGES: ObjectiveFactory;
var STAGES: ConstraintNetworkPruner;
var STAGES: Presolver;
var STAGES: NeighbourhoodFactory;
var STAGES: ArrayAccessOptimizer;

constraint all_different([
   DomainInitializer, VariableFactory, VariableClassifier, ConstraintFactory,
   ObjectiveFactory, ConstraintNetworkPruner, Presolver, NeighbourhoodFactory,
   ArrayAccessOptimizer]);

/* Sequence of stages to produce a complete model without cycles */
constraint DomainInitializer < VariableFactory;
constraint VariableFactory < VariableClassifier;
constraint VariableClassifier < ConstraintFactory;
constraint ConstraintFactory < CycleBreaker;

/* All remaining stages require a complete model without cycles */
constraint CycleBreaker < ObjectiveFactory;
constraint CycleBreaker < ConstraintNetworkPruner;
constraint CycleBreaker < Presolver;
constraint CycleBreaker < NeighbourhoodFactory;
constraint CycleBreaker < ArrayAccessOptimizer;

/* ObjectiveFactory generates and registers the objective variables */
constraint ObjectiveFactory < ConstraintNetworkPruner;

/* NeighbourhoodFactory departs from the objective produced by ObjectiveFactory */
constraint ObjectiveFactory < NeighbourhoodFactory;

/* Domains must not shrink after identification of implicit constraints */
constraint Presolver < NeighbourhoodFactory;

/* Enable generation of implicit from redundant constraints */
constraint NeighbourhoodFactory < ConstraintNetworkPruner;

/* Remove useless constraints before optimizing array access */
constraint ConstraintNetworkPruner < ArrayAccessOptimizer;
