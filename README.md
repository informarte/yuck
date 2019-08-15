# Yuck

[![Build Status](https://www.travis-ci.org/informarte/yuck.svg?branch=master)](https://www.travis-ci.org/informarte/yuck)

## Yuck in a nutshell

* Yuck is a [FlatZinc](http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf) interpreter that integrates with the [MiniZinc toolchain](http://www.minizinc.org/software).
* Yuck's approach to problem solving is based in local search.
* Yuck implements Boolean, integer, and integer set variables.
* Yuck implements many global constraints, see [FlatZinc support](#flatzinc-support).
* Yuck is provided under the terms of the [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0).
* In the [2017 MiniZinc challenge](http://www.minizinc.org/challenge2017/challenge.html), Yuck ranked second among local-search solvers.
* In the [2018 MiniZinc challenge](http://www.minizinc.org/challenge2018/challenge.html), Yuck defended its second place in the local-search category.

## Download and installation

Yuck packages are available from the [Releases page](https://github.com/informarte/yuck/releases); there are a Debian package (suitable for all Debian based systems, including Ubuntu and its offspring) and a ZIP package (suitable for all other systems). Moreover, a Docker image is available from [DockerHub](https://hub.docker.com/r/informarte/yuck).

When you installed the Debian package, you are already good to go; the package registers Yuck as a backend for the MiniZinc toolchain and no further manual setup is required.

When you decided for the ZIP package, proceed as follows:

1. Make sure that a [Java runtime environment](https://openjdk.java.net/install) is available on your system; Yuck requires at least version 8.
2. Unzip the package in a suitable location.
3. To register Yuck as a backend for the MiniZinc toolchain, define the ```MZN_SOLVER_PATH``` environment variable to point to the ```mzn``` subfolder of the Yuck distribution. (For other ways of providing a solver configuration file to the MiniZinc toolchain, see the section on [Solver Configuration Files](http://www.minizinc.org/doc-2.2.0/en/fzn-spec.html#solver-configuration-files) of *The MiniZinc Handbook*.)

The Docker image contains a Java runtime, the MiniZinc compiler and Yuck itself; it neither contains the MiniZinc IDE nor other solvers.

## Usage as MiniZinc backend

To apply Yuck to MiniZinc models, you need a working [MiniZinc](https://www.minizinc.org/software.html) installation. This section assumes that you have at least version 2.2.2 installed and that Yuck has been properly registered as a MiniZinc backend (see above).

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

Yuck provides dedicated solvers for the following global MiniZinc constraints:

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

When used as a FlatZinc interpreter, Yuck proceeds as follows:

* It performs a limited amount of constraint propagation to reduce variable domains before starting local search.
* It eliminates variables by exploiting equality constraints.
* It identifies and exploits functional dependencies to reduce the number of decision variables.
* It uses an annealing schedule that interleaves adaptive cooling with geometric reheating.
* In move generation, it concentrates on variables that are involved in constraint violations.
* It uses restarting to increase robustness: When a solver terminates without having reached its objective, it gets replaced by a new one starting out from another random assignment.
* When Yuck is configured to use multiple threads, restarting turns into parallel solving: Given a thread pool and a stream of solvers with a common objective, Yuck submits the solvers to the thread pool and, when one of the solvers provides a solution that satisfies the objective, Yuck discards all running and pending solvers.

## Future work

* Implement among, circuit, subcircuit, ...
* Provide soft constraints
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

Yuck tests are based on [JUnit 4](http://junit.org/junit4/) and
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
