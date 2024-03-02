package yuck.flatzinc.test

import org.junit.*
import org.junit.experimental.categories.Categories
import org.junit.experimental.categories.Categories.IncludeCategory
import org.junit.runner.RunWith
import org.junit.runners.Suite.SuiteClasses

import yuck.flatzinc.test.util.{HasAlldifferentConstraint, HasInverseConstraint, HasRegularConstraint}

/**
 * Big integration test suite
 *
 * @author Michael Marte
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
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasAlldifferentConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class AlldifferentChallenges


/**
 * Challenge problems with inverse constraints
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasInverseConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class InverseChallenges


/**
 * Challenge problems with regular constraints
 *
 * @author Michael Marte
 */
@RunWith(classOf[Categories])
@IncludeCategory(Array(classOf[HasRegularConstraint]))
@SuiteClasses(Array(classOf[MiniZincChallenges]))
class RegularChallenges
