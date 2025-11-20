package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.Categories
import org.junit.experimental.categories.Categories.IncludeCategory
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

import yuck.flatzinc.test.util.*

/**
 * Big integration test suite
 */
@runner.RunWith(classOf[runners.Suite])
@runners.Suite.SuiteClasses(
    Array(
        classOf[MiniZincExamples],
        classOf[MiniZincChallengeIntakeTests],
        classOf[MiniZincChallenges]))
class MiniZincTestSuites

/**
 * Challenge problems with all_different constraints
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasAllDifferentConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class AllDifferentChallenges

/**
 * Challenge problems with all_different_except constraints
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasAllDifferentExceptConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class AllDifferentExceptChallenges

/**
 * Challenge problems with inverse constraints
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasInverseConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class InverseChallenges


/**
 * Challenge problems with regular constraints
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasRegularConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class RegularChallenges
