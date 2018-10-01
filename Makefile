.PHONY: ci-tests unit-tests minizinc-tests minizinc-examples minizinc-challenges

ci-tests: yuck.test.ContinuousIntegrationTestSuite

unit-tests: yuck.test.UnitTestSuite

minizinc-tests: yuck.flatzinc.test.FlatZincImplementationTest

minizinc-examples: yuck.flatzinc.test.TractableMiniZincExamples

minizinc-challenges: yuck.flatzinc.test.MiniZincChallenges

yuck.test.% yuck.flatzinc.test.%:
	-sbt "test:runMain org.junit.runner.JUnitCore $@"

.PHONY: stage zip doc clean clean-tmp

stage:
	sbt stage

zip:
	sbt universal:packageBin

doc:
	sbt doc

clean:
	sbt clean

clean-tmp:
	-rm -fr tmp/*

render-readme:
	python3 -m grip
