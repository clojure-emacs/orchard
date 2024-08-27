.PHONY: submodules test quick-test docs eastwood cljfmt kondo install deploy clean lein-repl repl lint .EXPORT_ALL_VARIABLES
.DEFAULT_GOAL := install

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Ee

HOME=$(shell echo $$HOME)
CLOJURE_VERSION ?= 1.11
TEST_PROFILES ?= "-user,-dev,+test,+spec2,+cljs"

# The Lein profiles that will be selected for `lein-repl`.
# Feel free to upgrade this, or to override it with an env var named LEIN_PROFILES.
# Expected format: "+dev,+test"
# Don't use spaces here.
LEIN_PROFILES ?= "+dev,+test,+1.11,+cljs"

# The enrich-classpath version to be injected.
# Feel free to upgrade this.
ENRICH_CLASSPATH_VERSION="1.18.2"

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# We need Java sources to test Java parsing functionality, but the Docker images
# we use on CircleCI doesn't include src.zip. So we have to download them from
# Github and repackage in a form that is resemblant to src.zip from normal
# distributions.
base-src.zip:
	wget https://github.com/adoptium/jdk21u/archive/refs/tags/jdk-21.0.5+3.zip -O full-src.zip
	unzip -q full-src.zip
	cp -r jdk21u-*/src/java.base/share/classes java.base
	cp -r jdk21u-*/src/java.desktop/share/classes java.desktop
	zip -qr base-src.zip java.base java.desktop
	rm -rf java.base java.desktop jdk21u-* full-src.zip

test: clean submodules base-src.zip .EXPORT_ALL_VARIABLES
	@if [[ "$$PARSER_TARGET" == "parser-next" ]] ; then \
		lein with-profile $(TEST_PROFILES),+$(CLOJURE_VERSION),+parser-next test; \
	else \
		lein with-profile $(TEST_PROFILES),+$(CLOJURE_VERSION) test; \
	fi

# Sanity check that we don't break if Clojurescript or Spec2 aren't present.
test-no-extra-deps:
	lein with-profile -user,-dev,+test,+$(CLOJURE_VERSION) test

quick-test: test

eastwood:
	lein with-profile $(TEST_PROFILES),+$(CLOJURE_VERSION),+eastwood eastwood

cljfmt:
	lein with-profile -user,-dev,+$(CLOJURE_VERSION),+cljfmt cljfmt check

# Note that -dev is necessary for not hitting OOM errors in CircleCI
.make_kondo_prep: project.clj .clj-kondo/config.edn
	lein with-profile -dev,+test,+clj-kondo clj-kondo --copy-configs --dependencies --parallel --lint '$$classpath' > $@

kondo: .make_kondo_prep clean
	lein with-profile -dev,+test,+clj-kondo clj-kondo

lint: kondo cljfmt eastwood

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env clean
	lein with-profile -user,-dev,+$(CLOJURE_VERSION),-provided deploy clojars

# Usage: PROJECT_VERSION=99.99 make install
install: clean check-install-env
	lein with-profile -user,-dev,+$(CLOJURE_VERSION),-provided install

clean:
	lein with-profile -user,-dev clean

submodules: submodules/test-runner/deps.edn

submodules/test-runner/deps.edn:
	git submodule init
	git submodule update

.javac: $(wildcard test-java/orchard/*.clj)
	lein with-profile +test javac
	touch $@

# Create and cache a `java` command. project.clj is mandatory; the others are optional but are taken into account for cache recomputation.
# It's important not to silence with step with @ syntax, so that Enrich progress can be seen as it resolves dependencies.
.enrich-classpath-lein-repl: .javac submodules Makefile project.clj $(wildcard checkouts/*/project.clj) $(wildcard deps.edn) $(wildcard $(HOME)/.clojure/deps.edn) $(wildcard profiles.clj) $(wildcard $(HOME)/.lein/profiles.clj) $(wildcard $(HOME)/.lein/profiles.d) $(wildcard /etc/leiningen/profiles.clj)
	bash 'lein' 'update-in' ':plugins' 'conj' "[mx.cider/lein-enrich-classpath \"$(ENRICH_CLASSPATH_VERSION)\"]" '--' 'with-profile' $(LEIN_PROFILES) 'update-in' ':middleware' 'conj' 'cider.enrich-classpath.plugin-v2/middleware' '--' 'repl' | grep " -cp " > $@

# Launches a repl, falling back to vanilla lein repl if something went wrong during classpath calculation.
lein-repl: .enrich-classpath-lein-repl
	@if grep --silent " -cp " .enrich-classpath-lein-repl; then \
		export YOURKIT_SESSION_NAME="$(basename $(PWD))"; \
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
