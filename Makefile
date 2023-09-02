.PHONY: test quick-test docs eastwood cljfmt kondo install deploy clean .EXPORT_ALL_VARIABLES

VERSION ?= 1.11

TEST_PROFILES ?= +test

SPEC2_SOURCE_DIR = src-spec-alpha-2

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# .EXPORT_ALL_VARIABLES passes TEST_PROFILES to Lein so that it can inspect the active profiles, which is needed for a complete Eastwood setup:
test: clean $(SPEC2_SOURCE_DIR) .EXPORT_ALL_VARIABLES
	lein with-profile -user,-dev,+$(VERSION),$(TEST_PROFILES) test

quick-test: test

eastwood:
	lein with-profile -user,-dev,+$(VERSION),+eastwood,+deploy,$(TEST_PROFILES) eastwood

cljfmt:
	lein with-profile -user,-dev,+$(VERSION),+deploy,+cljfmt cljfmt check

# Note that -dev is necessary for not hitting OOM errors in CircleCI
.make_kondo_prep: project.clj .clj-kondo/config.edn
	lein with-profile -dev,+test,+clj-kondo,+deploy clj-kondo --copy-configs --dependencies --parallel --lint '$$classpath' > $@

kondo: .make_kondo_prep
	lein with-profile -dev,+test,+clj-kondo,+deploy clj-kondo

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env clean
	lein with-profile -user,-dev,+$(VERSION),-provided deploy clojars

# Usage: PROJECT_VERSION=0.14.2 make install
install: clean check-install-env
	lein with-profile -user,-dev,+$(VERSION),-provided install

clean:
	lein with-profile -user,-dev clean
	rm -rf $(SPEC2_SOURCE_DIR)

$(SPEC2_SOURCE_DIR):
	@if [ ! -d "$(SPEC2_SOURCE_DIR)" ]; then git clone https://github.com/clojure/spec-alpha2.git $(SPEC2_SOURCE_DIR) --depth=1; fi

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
