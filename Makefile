.PHONY: ci-tests unit-tests minizinc-tests minizinc-examples minizinc-challenges

ci-tests: yuck.test.ContinuousIntegrationTestSuite

unit-tests: yuck.test.UnitTestSuite

front-end-tests: yuck.flatzinc.test.FrontEndTestSuite

minizinc-examples: yuck.flatzinc.test.TractableMiniZincExamples

minizinc-challenges: yuck.flatzinc.test.MiniZincChallenges archive.minizinc-challenges

minizinc-benchmarks: yuck.flatzinc.test.MiniZincBenchmarks archive.minizinc-benchmarks

yuck.test.% yuck.flatzinc.test.%:
	./mill yuck.test.run $@

.PHONY: archive stage zip doc clean render-readme

COMMIT_DATE := $(shell git log -1 --pretty=format:%cd --date=format:%Y-%m-%d)
COMMIT_HASH := $(shell git rev-parse --short=8 HEAD)
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
NOW := $(shell date +%Y-%m-%d_%H-%M-%S)

archive.%: TAG = run-$(NOW)-$(subst /,-,$(BRANCH))-$(COMMIT_HASH)-$*
archive.%:
	cd logs && mkdir $(TAG) && mv ../tmp/* $(TAG) && tar cjf $(TAG).tar.bz2 $(TAG) && rm -fr $(TAG)
	git tag -f -m $(TAG) $(TAG)
	git push -f origin $(TAG)

stage:
	sbt stage

deb:
	sbt debian:lintian

zip:
	sbt universal:packageBin

doc:
	./mill yuck.docJar

clean:
	./mill clean

render-readme:
	python3 -m grip
