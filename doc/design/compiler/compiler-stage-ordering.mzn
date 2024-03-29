include "alldifferent.mzn";

int: N = 8;

set of int: STAGES = 1..N;

var STAGES: DomainInitializer;
var STAGES: VariableFactory;
var STAGES: VariableClassifier;
var STAGES: ConstraintFactory;
var STAGES: ObjectiveFactory;
var STAGES: ConstraintNetworkPruner;
var STAGES: Presolver;
var STAGES: NeighbourhoodFactory;

constraint alldifferent([
   DomainInitializer, VariableFactory, VariableClassifier, ConstraintFactory,
   ObjectiveFactory, ConstraintNetworkPruner, Presolver, NeighbourhoodFactory]);

/* Sequence of stages to produce a complete model */
constraint DomainInitializer < VariableFactory;
constraint VariableFactory < VariableClassifier;
constraint VariableClassifier < ConstraintFactory;

/* All remaining stages require a complete model */
constraint ConstraintFactory < ObjectiveFactory;
constraint ConstraintFactory < ConstraintNetworkPruner;
constraint ConstraintFactory < Presolver;
constraint ConstraintFactory < NeighbourhoodFactory;

/* ObjectiveFactory generates and registers the objective variables */
constraint ObjectiveFactory < ConstraintNetworkPruner;

/* NeighbourhoodFactory departs from the objective produced by ObjectiveFactory */
constraint ObjectiveFactory < NeighbourhoodFactory;

/* Domains must not shrink after identification of implicit constraints */
constraint Presolver < NeighbourhoodFactory;
