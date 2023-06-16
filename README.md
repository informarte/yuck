# Yuck

[![Build Status](https://www.travis-ci.org/informarte/yuck.svg?branch=master)](https://www.travis-ci.org/informarte/yuck)

## Yuck in a nutshell

* Yuck is a [FlatZinc](http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf) interpreter that integrates with the [MiniZinc toolchain](http://www.minizinc.org/software).
* Yuck's approach to problem solving is based in local search.
* Yuck implements Boolean, integer, and integer set variables, see [FlatZinc support](#flatzinc-support).
* Yuck implements many global constraints and their reified counterparts, see [Global constraints](#global-constraints).
* Yuck comes with support for vehicle routing, see [Extending Yuck for Vehicle Routing](https://github.com/informarte/yuck/releases/download/20210501/Extending_Yuck_for_Vehicle_Routing.pdf).
* Yuck features a mechanism to turn Boolean MiniZinc expressions (including applications of global constraints) into soft constraints, see [bool2costs](#bool2costs).
* Yuck supports lexicographic multi-objective optimization, see [Goal hierarchies](#goal-hierarchies).
* Yuck supports [warm starting](#warm-starting).
* Yuck is provided under the terms of the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0).
* Yuck has won several silver and gold medals at the [MiniZinc Challenge](https://www.minizinc.org/challenge.html).

## Contact

When you are using Yuck or you are considering to use it, and you have a question, want to report an issue, request a feature, share a success story, give feedback, or even get involved in development, then there are two ways to get into contact: Either raise an issue on the [Yuck issue tracker](https://github.com/informarte/yuck/issues) or send an email to yuck-solver@freenet.de.

## Download and installation

Yuck packages are available from the [Releases page](https://github.com/informarte/yuck/releases); there are a Debian package (suitable for all Debian based systems, including Ubuntu and its offspring) and a ZIP package (suitable for all other systems). Moreover, a Docker image is available from [DockerHub](https://hub.docker.com/r/informarte/yuck).

When you installed the Debian package, you are already good to go; the package registers Yuck as a backend for the MiniZinc toolchain and no further manual setup is required.

When you decided for the ZIP package, proceed as follows:

1. Make sure that a [Java runtime environment](https://openjdk.java.net/install) is available on your system; Yuck requires at least version 8.
2. Unzip the package in a suitable location.
3. To register Yuck as a backend for the MiniZinc toolchain, define the ```MZN_SOLVER_PATH``` environment variable to point to the ```mzn``` subfolder of the Yuck distribution. (For other ways of providing a solver configuration file to the MiniZinc toolchain, see the section on [Solver Configuration Files](http://www.minizinc.org/doc-2.7.5/en/fzn-spec.html#solver-configuration-files) of *The MiniZinc Handbook*.)
4. If you want to use Yuck on Windows with a MiniZinc version prior to 2.6.0, you need to take another step to work around a bug in MiniZinc: go to the ```mzn``` subfolder of the Yuck distribution, open the file ```yuck.msc```, find the line ```"executable": "../bin/yuck"``` and replace it with ```"executable": "../bin/yuck.bat"```.
5. If you want to use Yuck on MacOS, you have to install the `coreutils` package with the following Homebrew command: `brew install coreutils`

The Docker image contains an OpenJDK Java runtime, the MiniZinc compiler and Yuck itself; it neither contains the MiniZinc IDE nor other solvers.

## Usage as MiniZinc backend

To apply Yuck to MiniZinc models, you need a working [MiniZinc](https://www.minizinc.org/software.html) installation. This section assumes that you have at least version 2.7.5 installed and that Yuck has been properly registered as a MiniZinc backend (see above).

To use Yuck from inside the MiniZinc IDE, just select it from the menu of solver configurations before running your model.

To run MiniZinc models from the command line, use `minizinc` as follows:

```
minizinc --solver yuck zebra.mzn

zebra:
nation = [3, 4, 2, 1, 5]
colour = [3, 5, 4, 1, 2]
animal = [4, 1, 2, 5, 3]
drink  = [5, 2, 3, 4, 1]
smoke  = [3, 1, 2, 4, 5]
----------
```

## Direct usage

To use Yuck directly, invoke it with the FlatZinc file on the command line, for example:

```
yuck zebra.fzn

animal = array1d(0..4, [4, 1, 2, 5, 3]);
colour = array1d(0..4, [3, 5, 4, 1, 2]);
drink = array1d(0..4, [5, 2, 3, 4, 1]);
nation = array1d(0..4, [3, 4, 2, 1, 5]);
smoke = array1d(0..4, [3, 1, 2, 4, 5]);
----------
```

Yuck's output complies to the requirements of the [FlatZinc 1.6 specification](http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf) (see section 6).

Use the `--help` option to obtain a list of all options.

In case you need Yuck's MiniZinc library, its location depends on how you installed Yuck:

* When you installed the Debian package, the library resides in `/usr/share/yuck/mzn/lib`.
* When you installed the universal package, the library resides in the `mzn/lib` subfolder of the Yuck distribution.

## Using the Docker image

Say your home folder contains the directory `workspace` with the file `zebra.mzn` in it. To solve this problem by means of the Docker image, use the following command:

```
docker run -ti -v ~/workspace:/problems informarte/yuck:latest minizinc --solver yuck /problems/zebra.mzn
```

## Setting the maximum heap size

The maximum heap size defaults to 2 GB.

To increase the maximum heap size to, say, 4GB, use the Java command line option `-Xmx` as follows:

```
env JAVA_OPTS=-Xmx4g minizinc --solver yuck zebra.mzn
```

or

```
env JAVA_OPTS=-Xmx4g yuck zebra.fzn
```

## Under the hood

* Yuck's approach to problem solving is comparable to Comet [HM05] and OscaR/CBLS [BMFP15].
* Yuck implements simulated annealing along with some basic annealing schedules and some schedule combinators.
* Yuck supports lexicographic cost functions with both minimization and maximization goals.
* Yuck allows to timebox and parallelize solvers by means of solver combinators.
* Yuck supports the interruption and the resumption of solvers to facilitate the presentation of intermediate results.
* Yuck supports implicit solving by means of constraint-specific neighbourhoods.
* Yuck is written in Scala and exploits the Scala library's immutable collection classes for implementing global constraints.

## FlatZinc support

Yuck's FlatZinc front end supports all of FlatZinc except for float variables and float constraints.

Integers in Yuck are 64 bits wide.

Integer domains are represented as ranges or range lists.

If all integer-set variables are constrained to take their values from the powerset of [0, 63], values of integer-set variables are represented as 64-bit integers, otherwise they are represented as ranges or range lists.

When used as a FlatZinc interpreter, Yuck proceeds as follows:

* It eliminates variables by exploiting equality constraints.
* It identifies and exploits functional dependencies to reduce the number of decision variables.
* It prunes the constraint network by removing useless constraints.
* It prunes the search space by performing a limited amount of constraint propagation.
* It uses an annealing schedule that interleaves adaptive cooling with geometric reheating.
* In move generation, it concentrates on variables that are involved in constraint violations.
* It uses restarting to increase robustness: When a solver terminates without having reached its objective, it gets replaced by a new one starting out from another random assignment.
* When Yuck is configured to use multiple threads, restarting turns into parallel solving: Given a thread pool and a stream of solvers with a common objective, Yuck submits the solvers to the thread pool and, when one of the solvers provides a solution that satisfies the objective, Yuck discards all running and pending solvers.

## Global constraints

Yuck provides dedicated solvers for the following global MiniZinc constraints and their reified counterparts:

* all_different, all_different_except, all_different_except_0
* bin_packing, bin_packing_capa, bin_packing_load
* circuit
* count_eq, count_geq, count_gt, count_leq, count_lt, count_neq
* cumulative
* diffn, diffn_nonstrict
* disjunctive, disjunctive_strict
* global_cardinality, global_cardinality_closed, global_cardinality_low_up, global_cardinality_low_up_closed
* inverse
* lex_less, lex_lesseq
* maximum
* member
* minimum
* nvalue
* regular
* table

Yuck provides dedicated neighbourhoods for the following global MiniZinc constraints:

* all_different
* circuit
* inverse
* table: By default, only table constraints with not more than three columns are eligible for implicit solving. To disable this limitation for a particular table constraint, include `yuck.mzn` and annotate the constraint with `implicit`.

## MiniZinc extensions

### bool2costs

bool2costs is a function which measures how much the current assignment of values to problem variables violates a given Boolean MiniZinc expression. The smaller the violation, the lower the result and 0 means that the expression is satisfied.

bool2costs can be used to turn Boolean MiniZinc expressions into soft constraints, for example:

```
include "disjunctive.mzn";
include "yuck.mzn";

array [1..4] of var int: o;
array [1..4] of var int: d;

constraint o[1] in 2..5 /\ d[1] in 2..4;
constraint o[2] in 2..4 /\ d[2] in 1..6;
constraint o[3] in 3..6 /\ d[3] in 4..4;
constraint o[4] in 2..7 /\ d[4] in 1..3;

var int: overlap = bool2costs(disjunctive(o, d));

solve minimize(overlap);

output ["o = ", show(o), "\n", "d = ", show(d), "\n", "overlap = ", show(overlap)];
```

Applying Yuck to this problem results in:

```
o = [2, 4, 5, 2]
d = [2, 6, 4, 3]
overlap = 7
----------
o = [2, 4, 5, 7]
d = [2, 4, 4, 3]
overlap = 6
----------
o = [2, 4, 5, 7]
d = [2, 2, 4, 3]
overlap = 3
----------
o = [2, 4, 6, 4]
d = [2, 1, 4, 3]
overlap = 2
----------
o = [2, 4, 6, 4]
d = [2, 1, 4, 1]
overlap = 1
----------
o = [4, 2, 6, 3]
d = [2, 1, 4, 1]
overlap = 0
----------
==========
```

bool2costs is defined for every constraint implemented by Yuck, including all the global constraints listed above. Please see the section on [cost models](#cost-models) for the technical details.

To use bool2costs, you have to include `yuck.mzn`.

Keep in mind, though, that bool2costs is a non-standard MiniZinc extension which is not supported by other MiniZinc backends.

### Goal hierarchies

To state a lexicographic multi-objective optimization problem, define a goal hierarchy by annotating the solve statement as in the following example:

```
include "bin_packing_load_fn.mzn";
include "yuck.mzn";

array [1..6] of var 1..3: bin;
array [1..6] of int: weight = [i | i in 1..6];

constraint load = bin_packing_load(bin, weight);

constraint load[1] >= 3 /\ load[3] <= 10;

solve :: goal_hierarchy([int_min_goal(load[1]), int_min_goal(load[2]), int_min_goal(load[3])]) satisfy;

output ["bin = ", show(bin), "\n", "load = ", show(load)];
```

This MiniZinc program states: Find a solution that satisfies load[1] >= 3 and load[3] <= 10 and minimizes the load vector.

Applying Yuck to this problem yields:

```
bin = [2, 3, 2, 1, 2, 3]
load = [4, 9, 8]
----------
bin = [3, 3, 2, 1, 2, 3]
load = [4, 8, 9]
----------
bin = [3, 2, 3, 1, 2, 3]
load = [4, 7, 10]
----------
bin = [2, 3, 1, 3, 2, 2]
load = [3, 12, 6]
----------
bin = [3, 3, 1, 3, 2, 2]
load = [3, 11, 7]
----------
bin = [3, 2, 1, 3, 3, 2]
load = [3, 8, 10]
----------
```

The goal_hierarchy annotation accepts an unlimited number of goals. Apart from int_min_goal, there are int_max_goal and sat_goal.

Notice that the goal to satisfy the hard constraints is implicit and that it is the first and most important goal. All other goals g<sub>1</sub>, ..., g<sub>n</sub> are additional, lexicographically ordered optimization criteria: For 1 <= k < n, g<sub>k</sub> is strictly more important than g<sub>k + 1</sub>.

sat_goal can be used to define soft constraints as in the following example:

```
include "alldifferent.mzn";
include "yuck.mzn";

int: N = 10;

array [1..N] of var 1..N: x;

constraint x[1] = x[N];

solve :: goal_hierarchy([sat_goal(alldifferent(x))]) satisfy;

output ["x = ", show(x)];
```

This MiniZinc program states: Find a solution that satisfies x[1] = x[N] and minimizes the violation of the alldifferent constraint.

(Violations are measured by the [bool2costs](#bool2costs) function.)

Applying Yuck to this problem results in:

```
x = [8, 10, 9, 5, 1, 3, 2, 6, 7, 8]
----------
```

To use goal hierarchies, you have to include `yuck.mzn`.

Keep in mind, though, that goal hierarchies are a non-standard MiniZinc extension which are not supported by other MiniZinc backends.

### Warm starting

Yuck implements the `warm_start` and `warm_start_array` annotations.

Yuck will assign the given variables the given values in the given order, all other search variables will be assigned random values. Then Yuck will start simulated annealing from a lower temperature than normal to somewhat protect the given assignment until the first reheating.

Notice that Yuck will not automatically strive to minimize the changes to the given (partial) assignment. If this is a requirement, add an application-specific minimization goal, for example as part of a [goal hierarchy](#goal-hierarchies).

## Cost models

This section documents the cost models implemented by the [bool2costs](#bool2costs) function.

Notation:
- If x is a list or a set, then |x| denotes its size.
- If x is a variable, then σ(x) denotes its current value.

### Cost models for basic logical operations

Let p and q denote Boolean MiniZinc expressions.

| c | bool2costs(c) |
|---|---|
| p ∧ q | bool2costs(p) + bool2costs(q) |
| p ∨ q | if bool2costs(p) = 0 or bool2costs(q) = 0 then 0<br> else (bool2costs(p) + bool2costs(q)) / 2 |
| p xor q | if bool2costs(p) = 0 and bool2costs(q) = 0 then 1<br> else if bool2costs(p) > 0 and bool2costs(q) > 0 then (bool2costs(p) + bool2costs(q)) / 2<br> else 0 |
| p → q | if bool2costs(p) > 0 then 0 else (bool2costs(q) + 1) / 2 |
| p ↔ q | if bool2costs(p) = 0 and bool2costs(q) = 0 then 0<br> else if bool2costs(p) > 0 and bool2costs(q) > 0 then 0<br> else (bool2costs(p) + bool2costs(q) + 1) / 2 |
| ¬p | if bool2costs(p) > 0 then 0 else 1 |

### Cost models for basic integer operations

Let x and y denote integer variables.

| c | bool2costs(c) |
|---|---|
| x = y | abs(σ(x) - σ(y)) |
| x ≠ y | if σ(x) ≠ σ(y) then 0 else 1 |
| x < y | max(0, σ(x) - σ(y) + 1) |
| x ≤ y | max(0, σ(x) - σ(y)) |
| x > y | bool2costs(y < x) |
| x ≥ y | bool2costs(y ≤ x) |

### Cost models for basic set operations

Let s and t denote set variables.

| c | bool2costs(c) |
|---|---|
| s = t | bool2costs(s ⊆ t) + bool2costs(t ⊆ s) |
| s ≠ t | if σ(s) ≠ σ(t) then 0 else 1 |
| s < t | if σ(s) < σ(t) then 0 else 1 |
| s ≤ t | if σ(s) ≤ σ(t) then 0 else 1 |
| s > t | bool2costs(t < s) |
| s ≥ t | bool2costs(t ≤ s) |
| x ∈ s | if σ(s) is empty then 1 else distance of σ(x) to the nearest value in σ(s) |
| s ⊆ t | if σ(s) \ σ(t) is finite then \|σ(s) \ σ(t)\| else 1 |

### Cost models for global constraints

Notice that the following table covers only those global constraints which Yuck implements directly; the cost models of the other global constraints are determined by the standard decompositions of the MiniZinc library.

| c | bool2costs(c) |
|---|---|
| all_different([x<sub>1</sub>, ..., x<sub>n</sub>]) | n - \|{σ(x<sub>i</sub>): 1 ≤ i ≤ n}\| |
| all_different_except([x<sub>1</sub>, ..., x<sub>n</sub>], S) | \|[x<sub>i</sub>: 1 ≤ i ≤ n and σ(x<sub>i</sub>) ∉ S]\| -<br> \|{σ(x<sub>i</sub>): 1 ≤ i ≤ n and σ(x<sub>i</sub>) ∉ S}\| |
| all_different_except_0(x) | bool2costs(all_different_except(x, {0}) |
| bin_packing(capacity, bin, weight) | bool2costs(<br>&emsp; let { array [int] of var int: load = bin_packing_load(bin, weight); }<br>&emsp; in forall(i ∈ index_set(load))(load[i]  ≤ capacity)) |
| bin_packing_capa(capacity, bin, weight) | bool2costs(<br>&emsp; let { array [int] of var int: load = bin_packing_load(bin, weight); }<br>&emsp; in forall(i ∈ index_set(load))(load[i]  ≤ capacity[i])) |
| bin_packing_load(load, bin, weight) | bool2costs(<br>&emsp; forall(i ∈ index_set(load))(load[i] =<br>&emsp;&emsp; sum(j ∈ index_set(bin) where bin[j] = i)(weight[j])))|
| circuit(succ) | number of nodes - length of the longest cycle in the graph spanned by succ |
| count_eq(x, y, c) | bool2costs(c = sum(i ∈ index_set(x))(x[i] = y)) |
| count_neq(x, y, c) | bool2costs(c ≠ sum(i ∈ index_set(x))(x[i] = y)) |
| count_geq(x, y, c) | bool2costs(c ≥ sum(i ∈ index_set(x))(x[i] = y)) |
| count_gt(x, y, c) | bool2costs(c > sum(i ∈ index_set(x))(x[i] = y)) |
| count_leq(x, y, c) | bool2costs(c ≤ sum(i ∈ index_set(x))(x[i] = y)) |
| count_lt(x, y, c) | bool2costs(c < sum(i ∈ index_set(x))(x[i] = y)) |
| cumulative(...) | amount of unsatisfied resource requirements (summed up over time) |
| delivery(...) | sum of time-window violations,<br> see [Extending Yuck for Vehicle Routing](https://github.com/informarte/yuck/releases/download/20210501/Extending_Yuck_for_Vehicle_Routing.pdf) |
| diffn([x<sub>1</sub>, ..., x<sub>n</sub>], [y<sub>1</sub>, ..., y<sub>n</sub>], [w<sub>1</sub>, ..., w<sub>n</sub>], [h<sub>1</sub>, ..., h<sub>n</sub>]) | sum of rectangle overlaps considering empty rectangles:<br> sum<sub>1 ≤ i < j ≤ n</sub> \|A(i) ∩ A(j)\|<br> where A(i) = {(σ(x<sub>i</sub>) + k, σ(y<sub>i</sub>) + l): 0 ≤ k < d(w<sub>i</sub>) and 0 ≤ l < d(h<sub>i</sub>)}<br> and d(x) = if σ(x) = 0 then 1 else σ(x) |
| diffn_nonstrict([x<sub>1</sub>, ..., x<sub>n</sub>], [y<sub>1</sub>, ..., y<sub>n</sub>], [w<sub>1</sub>, ..., w<sub>n</sub>], [h<sub>1</sub>, ..., h<sub>n</sub>]) | sum of rectangle overlaps ignoring empty rectangles:<br> sum<sub>1 ≤ i < j ≤ n</sub> \|A(i) ∩ A(j)\|<br> where A(i) = {(σ(x<sub>i</sub>) + k, σ(y<sub>i</sub>) + l): 0 ≤ k < σ(w<sub>i</sub>) and 0 ≤ l < σ(h<sub>i</sub>)} |
| disjunctive_strict([s<sub>1</sub>, ..., s<sub>n</sub>], [d<sub>1</sub>, ..., d<sub>n</sub>]) | sum of task overlaps considering zero-duration tasks:<br> sum<sub>1 ≤ i < j ≤ n</sub> \|A(i) ∩ A(j)\|<br> where A(i) = {σ(s<sub>i</sub>) + k: 0 ≤ k < (if σ(d<sub>i</sub>)) = 0 then 1 else σ(d<sub>i</sub>))} |
| disjunctive([s<sub>1</sub>, ..., s<sub>n</sub>], [d<sub>1</sub>, ..., d<sub>n</sub>]) | sum of task overlaps ignoring zero-duration tasks:<br> sum<sub>1 ≤ i < j ≤ n</sub> \|A(i) ∩ A(j)\|<br> where A(i) = {σ(s<sub>i</sub>) + k: 0 ≤ k < σ(d<sub>i</sub>)} |
| global_cardinality(x, cover, count) | bool2costs(<br>&emsp; forall(i ∈ index_set(cover))(count_eq(x, cover[i], count[i]))) |
| global_cardinality_closed(x, cover, count) | bool2costs(<br>&emsp; forall(i ∈ index_set(x))(x[i] ∈ dom_array(cover)) ∧<br>&emsp; global_cardinality(x, cover, count)) |
| global_cardinality_low_up(x, cover, lb, ub) | bool2costs(<br>&emsp; let { array [int] of var int: count = global_cardinality(x, cover); }<br>&emsp; in forall(i in index_set(cover))(count[i] in lb[i]..ub[i]) |
| global_cardinality_low_up_closed(x, cover, lb, ub) | bool2costs(<br>&emsp; forall(i ∈ index_set(x))(x[i] ∈ dom_array(cover)) ∧<br>&emsp; global_cardinality_low_up(x, cover, lb, ub)) |
| inverse([f<sub>1</sub>, ..., f<sub>n</sub>], [g<sub>1</sub>, ..., g<sub>m</sub>]) | Σ<sub>1 ≤ i ≤ n</sub> \|σ(g<sub>σ(f<sub>i</sub>)</sub>) - i\| + Σ<sub>1 ≤ j ≤ m</sub> \|σ(f<sub>σ(g<sub>j</sub>)</sub>) - j\| |
| lex_less([x<sub>1</sub>, ..., x<sub>n</sub>], [y<sub>1</sub>, ..., y<sub>m</sub>]),<br> lex_lesseq([x<sub>1</sub>, ..., x<sub>n</sub>], [y<sub>1</sub>, ..., y<sub>m</sub>]) | if c is satisfied then 0<br> else if n > m and σ(x<sub>i</sub>) = σ(y<sub>i</sub>) for all 1 ≤ i ≤ m then 1<br> else min(n, m) - smallest index i with σ(x<sub>i</sub>) > σ(y<sub>i</sub>) |
| maximum(y, x) | bool2costs(y = max(x)) |
| member(x, y) | bool2costs(count_leq(x, y, 1)) |
| minimum(y, x) | bool2costs(y = min(x)) |
| nvalue(n, x) | bool2costs(n = nvalue(x)) |
| regular([x<sub>1</sub>, ..., x<sub>n</sub>], Q, S, δ, q<sub>0</sub>, F) | if q<sub>n</sub> ∈ F then 0<br> else n - length l of the longest prefix of the input sequence that could be extended to an acceptable sequence where<ul><li>q<sub>i</sub> = δ(q<sub>i - 1</sub>, σ(x<sub>i</sub>)) for 1 ≤ i ≤ n</li><li>1 ≤ l ≤ n is the smallest index such that d(q<sub>l</sub>) ≤ n - l or 0 if such an index does not exist</li><li>d(q) is the minimum number of transitions necessary to reach an acceptable state from state q or n if an acceptable state is unreachable from q</li></ul> |
| table(x, row) | min<sub>i ∈ index_set(row)</sub>(sum<sub>j ∈ index_set(x)</sub> bool2costs(x[j] = row[i][j])) |

## Future work

* Implement among, subcircuit, ...
* Provide libraries with Yuck core functionality

## Contributing

The Yuck build is based on [mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html). (There is no need to download mill when you run it with `./mill` from inside the Yuck root directory.) Morever, there is a Makefile with convenience targets most of which map to mill targets.

Run `make idea-project-files` to create project files for IntelliJ IDEA. Then, in IntelliJ IDEA, just import the Yuck root directory.

### Building

To build and rebuild Yuck and its documentation, use the following targets:

* `make compile` compiles all sources (including tests) that need compilation.
* `make doc` creates the ScalaDoc documentation from the sources.
* `make clean` removes all artifacts of compiling and building.

Notice that compilation requires a proper Git working copy.

### Testing

Yuck tests are based on [JUnit 4](http://junit.org/junit4/) and
[MiniZinc 2.7.5](http://www.minizinc.org/software.html).

* `make unit-tests` builds and runs all unit tests.
* `make front-end-tests` runs all FlatZinc front-end tests.
* `make minizinc-examples` exercises Yuck on a subset of the MiniZinc 1.6 examples.
* `make ci-tests` runs all these tests.

FlatZinc generation is fully automated and happens on the fly.

All test cases other than unit tests leave a log in the local `tmp`
folder.

Optimization results undergo automated verification using Gecode (part of the MiniZinc distribution).

### Running

There are various ways to run a development version of Yuck:

* Use `make run` to run Yuck from `mill` without integration with the MiniZinc toolchain.

* Build a package using `make deb` or `make zip` and use it as described in [Download and installation](#download-and-installation).

* Use `make stage` to create the script `out/yuck/launcher/dest/run`. To use this script with the MiniZinc toolchain, create a solver configuration file from the template `resources/mzn/yuck.msc.in` and move it a to a place where the MiniZinc toolchain can find it.

### Coding style

Yuck code should follow the [Scala Style
Guide](http://docs.scala-lang.org/style/) with two exceptions:

  * Indentation should be four instead of two spaces.
  * Method calls should always be chained with the dot operator, so don't use infix notation.

In addition, the following rules apply:

  * Lines should not be much longer than 120 characters.
  * Don't give a result type when overriding a definition.
  * Use `eq` and `ne` to compare to `null` because many classes have `==` and `!=` implementations which do not check for `null`.

## References

[BMFP15] G. Björdal, J.-N. Monette, P. Flener, and J. Pearson. A Constraint-Based Local Search Backend for MiniZinc. Constraints, 20(3):325-345, 2015. 

[HM05] P. V. Hentenryck and L. Michel. Constraint-Based Local Search. MIT Press, 2005.
