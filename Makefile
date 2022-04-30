.PHONY: ci-tests unit-tests minizinc-tests minizinc-examples minizinc-challenges

ci-tests: yuck.test.ContinuousIntegrationTestSuite

unit-tests: yuck.test.UnitTestSuite

hello-world-tests: yuck.test.HelloWorldTestSuite

front-end-tests: yuck.flatzinc.test.FrontEndTestSuite

minizinc-examples: yuck.flatzinc.test.TractableMiniZincExamples

minizinc-challenges: ci-tests
	./scripts/test-runner.py --mode ONE_JVM_PER_TEST_METHOD --archive yuck.flatzinc.test.MiniZincChallenges

minizinc-benchmarks: ci-tests
	./scripts/test-runner.py --archive yuck.flatzinc.test.MiniZincBenchmarks

yuck.test.% yuck.flatzinc.test.%:
	./scripts/test-runner.py $@

.PHONY: stage zip doc clean render-readme

idea-project-files:
	./mill mill.scalalib.GenIdea/idea

compile:
	./mill yuck.dev.test.compile

run:
	./mill yuck.dev.run

stage:
	./mill yuck.dev.launcher

deb:
	./mill yuck.dev.debianPackage

zip:
	./mill yuck.dev.universalPackage

doc:
	./mill yuck.dev.docJar

clean:
	./mill clean

render-readme:
	python3 -m grip
