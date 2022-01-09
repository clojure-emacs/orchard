.PHONY: test test-watch docs eastwood cljfmt release deploy clean .EXPORT_ALL_VARIABLES

VERSION ?= 1.10

TEST_PROFILES ?= +test

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# .EXPORT_ALL_VARIABLES passes TEST_PROFILES to Lein so that it can inspect the active profiles, which is needed for a complete Eastwood setup:
test: clean .EXPORT_ALL_VARIABLES
	lein with-profile -user,-dev,+$(VERSION),$(TEST_PROFILES) test

test-watch: test-resources/clojuredocs/export.edn clean
	lein with-profile +$(VERSION),$(TEST_PROFILES) test-refresh

eastwood:
	lein with-profile -user,-dev,+$(VERSION),+eastwood,$(TEST_PROFILES) eastwood

cljfmt:
	lein with-profile -user,-dev,+$(VERSION),+cljfmt cljfmt check

kondo:
	lein with-profile -user,-dev,+clj-kondo run -m clj-kondo.main --lint src test src-jdk8 src-newer-jdks test-newer-jdks test-cljs

# When releasing, the BUMP variable controls which field in the
# version string will be incremented in the *next* snapshot
# version. Typically this is either "major", "minor", or "patch".

BUMP ?= patch

release: clean
	lein with-profile -user,-dev,+$(VERSION),-provided release $(BUMP)

# Deploying requires the caller to set environment variables as
# specified in project.clj to provide a login and password to the
# artifact repository.
# Example:
# GIT_TAG=v0.9.0 CLOJARS_USERNAME=$USER CLOJARS_PASSWORD=$(pbpaste) make deploy
deploy: clean
	lein with-profile -user,-dev,+$(VERSION),-provided deploy clojars
	git tag -a "$$GIT_TAG" -m "$$GIT_TAG"
	git push
	git push --tags

install: clean
	lein with-profile -user,-dev,+$(VERSION),-provided install

clean:
	lein with-profile -user,-dev clean

check-env:
ifndef CLOJARS_USERNAME
	$(error CLOJARS_USERNAME is undefined)
endif
ifndef CLOJARS_PASSWORD
	$(error CLOJARS_PASSWORD is undefined)
endif
ifndef GIT_TAG
	$(error GIT_TAG is undefined)
endif
