package yuck.flatzinc.test

import org.junit.platform.suite.api.{IncludeTags, SelectClasses, Suite}

import yuck.flatzinc.test.util.HasGlobalConstraint.*

/**
 * Big integration test suite
 */
@Suite
@SelectClasses(
    Array(
        classOf[MiniZincExamples],
        classOf[MiniZincChallengeIntakeTests],
        classOf[MiniZincChallenges]))
class MiniZincTestSuites

/**
 * Challenge problems with all_different constraints
 */
@Suite
@SelectClasses(Array(classOf[MiniZincChallenges]))
@IncludeTags(Array(HasAllDifferentConstraint))
class AllDifferentChallenges

/**
 * Challenge problems with all_different_except constraints
 */
@Suite
@SelectClasses(Array(classOf[MiniZincChallenges]))
@IncludeTags(Array(HasAllDifferentExceptConstraint))
class AllDifferentExceptChallenges

/**
 * Challenge problems with inverse constraints
 */
@Suite
@SelectClasses(Array(classOf[MiniZincChallenges]))
@IncludeTags(Array(HasInverseConstraint))
class InverseChallenges

/**
 * Challenge problems with regular constraints
 */
@Suite
@SelectClasses(Array(classOf[MiniZincChallenges]))
@IncludeTags(Array(HasRegularConstraint))
class RegularChallenges
