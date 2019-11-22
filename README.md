# Yuck

[![Build Status](https://www.travis-ci.org/informarte/yuck.svg?branch=master)](https://www.travis-ci.org/informarte/yuck)

## Yuck in a nutshell

* Yuck is a [FlatZinc](http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf) interpreter that integrates with the [MiniZinc toolchain](http://www.minizinc.org/software).
* Yuck's approach to problem solving is based in local search.
* Yuck implements Boolean, integer, and integer set variables, see [FlatZinc support](#flatzinc-support).
* Yuck implements many global constraints and their reified counterparts, see [Global constraints](#global-constraints).
* Yuck features a mechanism to turn Boolean MiniZinc expressions (including applications of global constraints) into soft constraints, see [bool2costs](#bool2costs).
* Yuck is provided under the terms of the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0).
* In the [2017 MiniZinc challenge](http://www.minizinc.org/challenge2017/challenge.html), Yuck ranked second among local-search solvers.
* In the [2018 MiniZinc challenge](http://www.minizinc.org/challenge2018/challenge.html), Yuck defended its second place in the local-search category.

## Contact

When you are using Yuck or you are considering to use it, and you have a question, want to report an issue, request a feature, share a success story, give feedback, or even get involved in development, then there are two ways to get into contact: Either raise an issue on the [Yuck issue tracker](https://github.com/informarte/yuck/issues) or send an email to yuck-solver@freenet.de.

## Download and installation

Yuck packages are available from the [Releases page](https://github.com/informarte/yuck/releases); there are a Debian package (suitable for all Debian based systems, including Ubuntu and its offspring) and a ZIP package (suitable for all other systems). Moreover, a Docker image is available from [DockerHub](https://hub.docker.com/r/informarte/yuck).

When you installed the Debian package, you are already good to go; the package registers Yuck as a backend for the MiniZinc toolchain and no further manual setup is required.

When you decided for the ZIP package, proceed as follows:

1. Make sure that a [Java runtime environment](https://openjdk.java.net/install) is available on your system; Yuck requires at least version 8.
2. Unzip the package in a suitable location.
3. To register Yuck as a backend for the MiniZinc toolchain, define the ```MZN_SOLVER_PATH``` environment variable to point to the ```mzn``` subfolder of the Yuck distribution. (For other ways of providing a solver configuration file to the MiniZinc toolchain, see the section on [Solver Configuration Files](http://www.minizinc.org/doc-2.3.2/en/fzn-spec.html#solver-configuration-files) of *The MiniZinc Handbook*.)

The Docker image contains an OpenJDK Java runtime, the MiniZinc compiler and Yuck itself; it neither contains the MiniZinc IDE nor other solvers.

## Usage as MiniZinc backend

To apply Yuck to MiniZinc models, you need a working [MiniZinc](https://www.minizinc.org/software.html) installation. This section assumes that you have at least version 2.3.2 installed and that Yuck has been properly registered as a MiniZinc backend (see above).

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

By default, the Docker image limits the maximum heap size to 2 GB.

## Setting the maximum heap size

To set the maximum heap size to, say, 4GB, use the Java command line option `-Xmx` as follows:

```
minizinc --solver yuck --fzn-flags -J-Xmx4g zebra.mzn
```

or

```
yuck -J-Xmx4g zebra.fzn
```

## Under the hood

* Yuck's approach to problem solving is comparable to Comet [HM05] and [OscaR](http://oscarlib.bitbucket.org)/CBLS [BMFP15].
* Yuck implements simulated annealing along with some basic annealing schedules and some schedule combinators.
* Yuck supports lexicographic cost functions with both minimization and maximization goals.
* Yuck allows to timebox and parallelize solvers by means of solver combinators.
* Yuck supports the interruption and the resumption of solvers to facilitate the presentation of intermediate results.
* Yuck supports implicit solving by means of constraint-specific neighbourhoods.
* Yuck is written in Scala and exploits the Scala library's immutable collection classes for implementing global constraints.

## FlatZinc support

Yuck's FlatZinc frontend supports all of FlatZinc except for float variables and float constraints.

When used as a FlatZinc interpreter, Yuck proceeds as follows:

* It performs a limited amount of constraint propagation to reduce variable domains before starting local search.
* It eliminates variables by exploiting equality constraints.
* It identifies and exploits functional dependencies to reduce the number of decision variables.
* It uses an annealing schedule that interleaves adaptive cooling with geometric reheating.
* In move generation, it concentrates on variables that are involved in constraint violations.
* It uses restarting to increase robustness: When a solver terminates without having reached its objective, it gets replaced by a new one starting out from another random assignment.
* When Yuck is configured to use multiple threads, restarting turns into parallel solving: Given a thread pool and a stream of solvers with a common objective, Yuck submits the solvers to the thread pool and, when one of the solvers provides a solution that satisfies the objective, Yuck discards all running and pending solvers.

## Global constraints

Yuck provides dedicated solvers for the following global MiniZinc constraints and their reified counterparts:

* all_different, all_different_except_0
* at_least, at_most
* bin_packing, bin_packing_capa, bin_packing_load
* count_eq, count_geq, count_gt, count_leq, count_lt, count_neq
* cumulative
* diffn, diffn_nonstrict
* disjunctive, disjunctive_strict
* exactly
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
* inverse

## bool2costs

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

output [
    "o = ", show(o), "\n",
    "d = ", show(d), "\n",
    "overlap = ", show(overlap)];
```

Applying Yuck to this problem will result in:

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

bool2costs is defined for every constraint implemented by Yuck, including all the global constraints listed above. The underlying cost models are not yet documented in detail but most of them are quite intuitive. For example:

- bool2costs(*x* = *y*) = abs(value assigned to *x* - value assigned to *y*)
- bool2costs(e1 /\ e2) = bool2costs(e1) + bool2costs(e2)
- bool2costs(disjunctive(s, d)) computes how much each pair of tasks overlaps and returns the sum of these overlaps.

To use bool2costs, you have to include `yuck.mzn`.

Keep in mind, though, that bool2costs is a non-standard MiniZinc extension which is not supported by other MiniZinc backends.

## Future work

* Implement among, circuit, subcircuit, ...
* Reduce dependence on integration testing by adding more unit tests
* Provide libraries with Yuck core functionality

## Contributing

To build and run Yuck, you need [sbt](http://www.scala-sbt.org).

Run `sbt eclipse` to create an Eclipse project.

### Building

To build and rebuild Yuck and its documentation, use the sbt standard targets:

* `sbt compile` compiles all sources that need compilation.
* `sbt doc` creates the ScalaDoc documentation from the sources.
* `sbt clean` removes all artifacts of compiling and building.

### Testing

Yuck tests are based on [JUnit 4](http://junit.org/junit4/),
[ScalaMock](http://scalamock.org), and
[MiniZinc 2.3.x](http://www.minizinc.org/software.html).

* `make unit-tests` builds and runs all unit tests.
* `make minizinc-tests` runs all FlatZinc frontend tests.
* `make minizinc-examples` exercises Yuck on a subset of the MiniZinc 1.6 examples.
* `make ci-tests` runs all these tests.

FlatZinc generation is fully automated and happens on the fly.

All test cases other than unit tests leave a log in the local ```tmp```
folder.

Optimization results undergo automated verification using Gecode (part of the MiniZinc distribution).

### Running

There are two ways to run Yuck:

* Stage and run it:

  ```
   sbt stage
   ./bin/yuck --help
   ```

* Run it through sbt:

  ```
  sbt run --help
  ```

### Coding style

Yuck code should follow the [Scala Style
Guide](http://docs.scala-lang.org/style/) with two exceptions:

  * Indentation should be four instead of two spaces.
  * Method calls should always be chained with the dot operator, so don't use infix notation.

In addition, the following rules apply:

  * Lines should not be much longer than 120 characters.
  * Don't give a result type when overriding a definition.

## References

[BMFP15] G. Björdal, J.-N. Monette, P. Flener, and J. Pearson. A Constraint-Based Local Search Backend for MiniZinc. Constraints, 20(3):325-345, 2015. 

[HM05] P. V. Hentenryck and L. Michel. Constraint-Based Local Search. MIT Press, 2005.
