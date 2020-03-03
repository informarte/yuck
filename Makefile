include Makefile.common
include Makefile.common.test

.PHONY: ci-tests unit-tests minizinc-tests minizinc-examples minizinc-challenges

ci-tests: yuck.test.ContinuousIntegrationTestSuite

unit-tests: yuck.test.UnitTestSuite

hello-world-tests: yuck.test.HelloWorldTestSuite

front-end-tests: yuck.flatzinc.test.FrontEndTestSuite

minizinc-examples: yuck.flatzinc.test.TractableMiniZincExamples

minizinc-challenges: yuck.flatzinc.test.MiniZincChallenges archive.minizinc-challenges

minizinc-benchmarks: yuck.flatzinc.test.MiniZincBenchmarks archive.minizinc-benchmarks

.PHONY: yuck.test.% yuck.flatzinc.test.%
yuck.test.% yuck.flatzinc.test.%:
	./mill yuck.test.run $@

.PHONY: idea-project-files
idea-project-files:
	./mill mill.scalalib.GenIdea/idea

.PHONY: compile
compile:
	./mill yuck.test.compile

.PHONY: run
run:
	./mill yuck.run

.PHONY: stage
stage:
	./mill yuck.launcher

.PHONY: deb
deb:
	./mill yuck.debianPackage

.PHONY: zip
zip:
	./mill yuck.universalPackage

.PHONY: doc
doc:
	./mill yuck.docJar
