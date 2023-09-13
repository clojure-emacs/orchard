.PHONY: test quick-test docs eastwood cljfmt kondo install deploy clean lein-repl repl .EXPORT_ALL_VARIABLES
.DEFAULT_GOAL := install

HOME=$(shell echo $$HOME)
VERSION ?= 1.11
TEST_PROFILES ?= +test
SPEC2_SOURCE_DIR = src-spec-alpha-2

# The Lein profiles that will be selected for `lein-repl`.
# Feel free to upgrade this, or to override it with an env var named LEIN_PROFILES.
# Expected format: "+dev,+test"
# Don't use spaces here.
LEIN_PROFILES ?= "+dev,+test,+1.11"

# The enrich-classpath version to be injected.
# Feel free to upgrade this.
ENRICH_CLASSPATH_VERSION="1.17.0"

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

kondo: .make_kondo_prep clean
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

.javac: $(wildcard test-java/orchard/*.clj)
	lein with-profile +test javac
	touch $@

# Create and cache a `java` command. project.clj is mandatory; the others are optional but are taken into account for cache recomputation.
# It's important not to silence with step with @ syntax, so that Enrich progress can be seen as it resolves dependencies.
.enrich-classpath-lein-repl: .javac $(SPEC2_SOURCE_DIR) Makefile project.clj $(wildcard checkouts/*/project.clj) $(wildcard deps.edn) $(wildcard $(HOME)/.clojure/deps.edn) $(wildcard profiles.clj) $(wildcard $(HOME)/.lein/profiles.clj) $(wildcard $(HOME)/.lein/profiles.d) $(wildcard /etc/leiningen/profiles.clj)
	bash 'lein' 'update-in' ':plugins' 'conj' "[mx.cider/lein-enrich-classpath \"$(ENRICH_CLASSPATH_VERSION)\"]" '--' 'with-profile' $(LEIN_PROFILES) 'update-in' ':middleware' 'conj' 'cider.enrich-classpath.plugin-v2/middleware' '--' 'repl' | grep " -cp " > $@

# Launches a repl, falling back to vanilla lein repl if something went wrong during classpath calculation.
lein-repl: .enrich-classpath-lein-repl
	@if grep --silent " -cp " .enrich-classpath-lein-repl; then \
		eval "$$(cat .enrich-classpath-lein-repl) --interactive"; \
	else \
		echo "Falling back to lein repl... (you can avoid further falling back by removing .enrich-classpath-lein-repl)"; \
		lein with-profiles $(LEIN_PROFILES) repl; \
	fi

repl: lein-repl

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
