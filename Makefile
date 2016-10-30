tests: unit-tests minizinc-tests minizinc-examples

unit-tests: yuck.test.UnitTestSuite

minizinc-tests: yuck.flatzinc.test.MiniZincTests

minizinc-examples: yuck.flatzinc.test.TractableMiniZincExamples

minizinc-challenges: yuck.flatzinc.test.MiniZincChallenges

yuck.test.% yuck.flatzinc.test.%:
	-sbt "test:run-main org.junit.runner.JUnitCore $@"

stage:
	sbt stage

zip:
	sbt universal:package-bin

doc:
	sbt doc

clean-tmp:
	-rm -fr tmp/*
