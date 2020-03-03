include Makefile.common
include Makefile.common.test

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
