.PHONY: test docs eastwood cljfmt deploy clean .EXPORT_ALL_VARIABLES

VERSION ?= 1.10

TEST_PROFILES ?= +test

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# .EXPORT_ALL_VARIABLES passes TEST_PROFILES to Lein so that it can inspect the active profiles, which is needed for a complete Eastwood setup:
test: clean .EXPORT_ALL_VARIABLES
	lein with-profile -user,-dev,+$(VERSION),$(TEST_PROFILES) test

eastwood:
	lein with-profile -user,-dev,+$(VERSION),+eastwood,+deploy,$(TEST_PROFILES) eastwood

cljfmt:
	lein with-profile -user,-dev,+$(VERSION),+deploy,+cljfmt cljfmt check

kondo:
	lein with-profile -user,-dev,+clj-kondo run -m clj-kondo.main --lint src test src-jdk8 src-newer-jdks test-newer-jdks test-cljs .circleci/deploy

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env clean
	lein with-profile -user,-dev,+$(VERSION),-provided deploy clojars

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
ifndef CIRCLE_TAG
	$(error CIRCLE_TAG is undefined. Please only perform deployments by publishing git tags. CI will do the rest.)
endif

check-install-env:
ifndef PROJECT_VERSION
	$(error Please set PROJECT_VERSION as an env var beforehand.)
endif
