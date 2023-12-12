.PHONY: ci-tests unit-tests minizinc-tests minizinc-examples minizinc-challenges

ci-tests: yuck.test.ContinuousIntegrationTestSuite

unit-tests: yuck.test.UnitTestSuite

hello-world-tests: yuck.test.HelloWorldTestSuite

front-end-tests: yuck.flatzinc.test.FrontEndTestSuite

minizinc-examples: yuck.flatzinc.test.TractableMiniZincExamples

minizinc-challenge-intake-tests: yuck.flatzinc.test.MiniZincChallengeIntakeTests

minizinc-challenges: ci-tests
	./scripts/test-runner.py --mode ONE_JVM_PER_TEST_METHOD --archive yuck.flatzinc.test.MiniZincChallenges

minizinc-benchmarks: ci-tests
	./scripts/test-runner.py --archive yuck.flatzinc.test.MiniZincBenchmarks

yuck.test.% yuck.flatzinc.test.%:
	./scripts/test-runner.py $@

.PHONY: bsp idea-project-files compile run stage zip doc clean render-readme

bsp:
	./mill mill.bsp.BSP/install

idea-project-files:
	./mill mill.idea.GenIdea/idea

compile:
	./mill yuck.test.compile

run:
	./mill yuck.run

stage:
	./mill yuck.launcher

deb:
	./mill yuck.debianPackage

zip:
	./mill yuck.universalPackage

doc:
	./mill yuck.docJar

clean:
	./mill clean

render-readme:
	python3 -m grip
